000100120727     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PRNPGM') ACTGRP('QILE')
000200120727     H CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500110506     FFiapd01L  iF   E           K DISK
000600110506     FanRCO98j  iF   E           k disk
000700110506      *
000800090331     FAITRS00f  UF A E             DISK    commit
000900090331     FAitra00f  uF a E             DISK    commit
001000090331      *
001100090331     FAitra01l  uF   E           k DISK    commit
001200090331     F                                     rename(Aitra000:Aitra001)
001300090401      *
001400110506     FTRMZ70SD3 CF   E             WORKSTN usropn
001500110506     F                                     SFILE(S1:S1NRR)
001600110506     F                                     INFDS(DSFMT)
001700110509     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
001800110506     D*----------------------------------------------------*
001900110509     D STT             S             66    DIM(2) CTDATA PERRCD(1)              STAMPA
002000110509     D*-------------
002100110506     D DSFMT           DS           512
002200110506     D  $TASTO               369    369
002300110506     D  NRG                  370    370
002400110506     D  NCL                  371    371
002500110506     D  SFLNRR               378    379B 0
002600110506     D*-------------
002700110506     D* Reperimento nome PGM
002800110506     D STATUS         SDS           333
002900110506     D* Nome PGM a video
003000110506     D  DSPGM            *PROC
003100110506     D*-------------
003200090401     D KPJBA         E DS
003300090401     D KPJBus          S                   like(KPJBU)
003400090401     D trul33ds      E DS
003500090401     D savNRC          s                   like(trsNRC)
003600090401     D da_duplicare    s              1
003700110505     D da_segnalare    s              1
003800120727     D daCODaut        s              7  0
003900120727     D  aCODaut        s              7  0
004000110506     D Codici_da_segnalare...
004100110506     d                 s              1    INZ('N')
004200120727     D gia_presente_contratto...
004300120727     d                 s              1    INZ('N')
004400120727     D Tutti_Di_una_sola_Filiale...
004500120727     d                 s              1    INZ('N')
004600101102     d dataiso         s               d   datfmt(*iso)
004700101102     d dataeur         s               d   datfmt(*eur)
004800101102     d Data_oggi       s              8  0
004900090401     D*-------------
005000110506     D KAU_Segnalare   S              7  0 DIM(100)
005100110506     D KAU_RagSoc      S             35    DIM(100)
005200110506     D BPT_Segnalare   S              7  0 DIM(100)
005300110506     D BPT_RagSoc      S             35    DIM(100)
005400090408     D trmz70s1ds    E DS                  prefix(S1_)
005500110506     D LIMITE          s              3  0 INZ(100)
005600110506     D RrnLast         S              5I 0
005700110506     D*-------------
005800110506     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005900110506$003 D S1NRR           S                   Like(C1rcd)
006000110506$003 D WSfl            S                   Like(C1nrr)
006100110506$003 D Wmax            S                   Like(C1rcd)
006200110506$003 D Wpag            S                   Like(C1rcd)
006300110506$003 D Winzs1          S                   Like($inzs1)
006400110506     D*-------------
006500110506     D* COSTANTI
006600110506     D*-------------
006700110506     D SFLPAG          C                   CONST(11)
006800110506     D* dimensione della schiera $MS1
006900110506     D*
007000110506     D* Tasti di funzione
007100110506     D F01             C                   CONST(X'31')
007200110506     D F02             C                   CONST(X'32')
007300110506     D F03             C                   CONST(X'33')
007400110506     D F04             C                   CONST(X'34')
007500110506     D F05             C                   CONST(X'35')
007600110506     D F06             C                   CONST(X'36')
007700110506     D F07             C                   CONST(X'37')
007800110506     D F08             C                   CONST(X'38')
007900110506     D F09             C                   CONST(X'39')
008000110506     D F10             C                   CONST(X'3A')
008100110506     D F11             C                   CONST(X'3B')
008200110506     D F12             C                   CONST(X'3C')
008300110506     D F13             C                   CONST(X'B1')
008400110506     D F14             C                   CONST(X'B2')
008500110506     D F15             C                   CONST(X'B3')
008600110506     D F16             C                   CONST(X'B4')
008700110506     D F17             C                   CONST(X'B5')
008800110506     D F18             C                   CONST(X'B6')
008900110506     D F19             C                   CONST(X'B7')
009000110506     D F20             C                   CONST(X'B8')
009100110506     D F21             C                   CONST(X'B9')
009200110506     D F22             C                   CONST(X'BA')
009300110506     D F23             C                   CONST(X'BB')
009400110506     D F24             C                   CONST(X'BC')
009500110506     D ENTER           C                   CONST(X'F1')
009600110506     D ROLDWN          C                   CONST(X'F4')
009700110506     D ROLLUP          C                   CONST(X'F5')
009800120727     D Cambio_Societa  C                   CONST('S')
009900120727     D Cambio_PIVA     C                   CONST('P')
010000120727     D Data_FineContr  C                   CONST('D')
010100120727     D SI              C                   CONST('S')
010200120727     D NO              C                   CONST('N')
010300110506      **********************************************************************
010400090401      *
010500090401     C     *ENTRY        PLIST
010600110506     c                   parm                    kpjba
010700090401     C                   parm                    tipo_aggior       1
010800090401     C                   parm                    Data_Fine         8 0
010900090401     C                   parm                    Data_Inizio       8 0
011000090401     C                   parm                    Societa           3
011100090401     C                   parm                    DesSocieta       45
011200090401     C                   parm                    PartitaIva       16
011300110505     C                   parm                    Filiale           3 0
011400090401     C                   parm                    NRec_TRS          9 0
011500090401     C                   parm                    Errore            1
011600120727      **
011700090401     C                   clear                   Errore
011800120727      **
011900120727      ** prepara il range per i test successivi sui codici AUT
012000120727     C                   z-add     0             daCODaut
012100120727     C                   z-add     9999999       aCODaut
012200120727     c                   movel     Filiale       daCODaut
012300120727     c                   movel     Filiale       aCODaut
012400110506     C*
012500110506     c     Kaut          klist
012600110506     c                   kfld                    tipo              1
012700110506     c                   kfld                    autista           7 0
012800110506     C*
012900110506     C     Krco98j       KLIST
013000110506     C                   KFLD                    RCOSOCIETA
013100110506     C                   KFLD                    subnatura         1
013200110506     C                   KFLD                    RCOKSC
013300110506     C*
013400101102     C*  giro la udate da GMA in AMG
013500101102     C                   move      *date         dataeur
013600101102     C                   move      dataeur       dataiso
013700101102     C                   move      dataiso       data_oggi
013800101102     C*
013900120727     c                   eval      Codici_da_segnalare =  NO
014000090401      *
014100120727     c                   if        tipo_aggior = Cambio_Societa
014200090401     c                   exsr      Cambia_Db2
014300090401      *
014400120727     c                   elseIf    tipo_aggior = Cambio_PIVA
014500090401     c                   exsr      Cambia_Db2
014600090401      *
014700120727     c                   elseIf    tipo_aggior = Data_FineContr
014800090401     c                   exsr      DataFineC_Db2
014900090401     c                   end
015000090331      *
015100110506      *  se deve essere segnalato qualcosa apre la finestra da far vedere
015200120727     c                   if        Codici_da_segnalare =  SI
015300110506      *
015400110506     C                   eval      $INZS1 = *ON
015500110506     C                   MOVE      *OFF          $FINE
015600110506     C                   MOVE      'S1'          $GEST
015700110506      *
015800110506     c                   open      trmz70sd3
015900110506     C*
016000110506     C     $FINE         DOWEQ     *OFF
016100110506     C     $GEST         CASEQ     'S1'          GESS1
016200110506     C                   END
016300110506     C                   END
016400110506      *
016500110506     c                   close     trmz70sd3
016600110506     c                   end
016700110506      *
016800090401     c                   seton                                        LR
016900090331     C************************************************************
017000090401     C* Chiude il contratto e lo riapre
017100090331     C************************************************************
017200090401     C     Cambia_DB2    BEGSR
017300090331     C*
017400110506     C* Clienti da segnalare a video in seguito
017500110506     c                   clear                   KAU_Segnalare
017600110506     c                   clear                   KAU_RagSoc
017700110506     c                   clear                   BPT_Segnalare
017800110506     c                   clear                   BPT_RagSoc
017900110506     c                   z-add     0             NrKAU             3 0
018000110506     c                   z-add     0             NrBPT             3 0
018100110506     C*
018200090401     C* imposta la data di chiusura contratto con la vecchia P.IVA
018300090401     C*  e riapre un nuovo contratto con la nuova P.IVA.
018400090401     C* Disaccredita tutti gli AUT legati al vecchio contratto
018500090401     C*  e li riaccredita sul nuovo contratto.
018600090401     C     NRec_TRS      chain     aitrs00f
018700090401     c                   z-add     trsNRC        savNRC
018800120727      *
018900120727      * Solo per il cambio SOCIETà
019000120727      *  Attenzione: occorre far scadere solo se al contratto sono legati
019100120727      *   gli autisti di una sola filiale,  altrimenti NON DEVE FARE SCADERE
019200120727      *   il vecchio CONTRATTO.
019300120727     c                   if        tipo_aggior = Cambio_Societa
019400120727      **
019500120727     c                   eval      Tutti_Di_una_sola_Filiale =  SI
019600120727     C/EXEC SQL
019700120727     C+ SELECT  'N' into :Tutti_Di_una_sola_Filiale
019800120727     C+   FROM  AITRA00F
019900120727     C+  WHERE  TRANRC = :trsnrc and tradfi=0  and
020000120727     C+   (
020100120727     C+  (trakau > 0 and (trakau < :daCODaut or trakau > :aCODaut) )
020200120727     C+    or
020300120727     C+  (trabpt > 0 and (trabpt < :daCODaut or trabpt > :aCODaut) )
020400120727     C+   )
020500120727     C/END-EXEC
020600120727     c                   if        Tutti_Di_una_sola_Filiale = SI
020700120727     c                   eval      trsDFC = Data_Fine
020800120727     c                   end
020900120727      **
021000120727     c                   end
021100120727      *
021200120727      *  x il Cambio P.IVA invece sempre
021300120727     c                   if        tipo_aggior = Cambio_PIVA
021400090401     c                   eval      trsDFC = Data_Fine
021500120727     c                   end
021600120727      *
021700110505     c                   update    aitrs000                             79
021800090331     C* se rcd non scrivibile attivo errore
021900090401     C   79              move      *on           Errore
022000090401    >C   79              leavesr
022100090401      *
022200120727      *   Controlla se presente un contratto sulla nuova società
022300120727      *   con stessa P.IVA
022400120727      **
022500120727     c                   eval      gia_presente_contratto = NO
022600120727     C/EXEC SQL
022700120727     C+ SELECT 'S', trsNRC
022800120727     C+   into
022900120727     C+        :gia_presente_contratto, :NUMERO_nrc
023000120727     C+   FROM  AITRS00F
023100120727     C+  WHERE  TRSSOCG = :Societa     and
023200120727     C+         TRSIVA  = :PartitaIva  and
023300120727     C+         TRSdfc  = 0 and TRSann = ' '
023400120727     C/END-EXEC
023500120727      *
023600120727      *   Se si deve fare il Cambio SOCIETA NON deve già essere presente
023700120727      *   il contratto altrimenti deve prendere il contratto già PRESENTE.
023800120727      *
023900120727     c                   if        tipo_aggior = Cambio_Societa and
024000120727     c                             gia_presente_contratto = NO
024100120727     c                               or
024200120727     c                             tipo_aggior = Cambio_PIVA
024300120727      *
024400090401      *  quindi scrive un altro record su TRS con un nuovo contratto
024500090401     c                   exsr      repNUM
024600090401     c                   z-add     NUMERO_nrc    trsNRC
024700090401     c                   eval      trsDeC = Data_Inizio
024800090401     c                   eval      trsDfC = 0
024900090401      *
025000120727     c                   if        tipo_aggior = Cambio_Societa
025100090401     c                   eval      trssocG= Societa
025200110505     c                   eval      trsFIL = Filiale
025300110505      *
025400120727     c                   elseIf    tipo_aggior = Cambio_PIVA
025500090401     c                   eval      trsIVA = PartitaIva
025600090401     c                   end
025700090401      *
025800120731     c                   clear                   trsflr
025900101102     c                   eval      trsDRC = 0
026000101102     c                   eval      trsDSC = 0
026100101102     c                   eval      TRSDIM = DATA_OGGI
026200101102     c                   eval      TRSDVA = 0
026300101102     c                   eval      TRSUTE = KNMUS
026400101102      *
026500090401     c                   write     aitrs000                             79
026600090401     C   79              move      *on           Errore
026700090401    >C   79              leavesr
026800090331      *
026900120727     c                   endIF
027000120727      *
027100090331      *  Deve annullare tutti i record collegati di AITRA a quel contratto
027200090401     c     SAVnrc        setll     aitra01l
027300090401     c     SAVnrc        reade     aitra01l
027400090331      *
027500120727     c                   Dow       not %EoF(aitra01l)
027600120727      *
027700120727     c                   IF            tipo_aggior = Cambio_PIVA
027800120727     c                              or
027900120727     c                                 tipo_aggior = Cambio_Societa and
028000120727     c                             (trakau >= daCODaut and trakau <= aCODaut
028100120731     c                              and trakau > 0
028200120727     c                                 or
028300120731     c                              trabpt >= daCODaut and trabpt <= aCODaut
028400120731     c                              and trabpt > 0
028500120731     c                                 or
028600120801     c                              trasco <> ' ' and trafil = filiale)
028700090331      *
028800120727     c                   eval      da_duplicare =  NO
028900120727     c                   eval      da_segnalare =  NO
029000090331      *
029100090331      *  Solo se non era annullato o già disaccreditato
029200090331     c                   if        traDFI = 0 and traANN = *blank
029300110505      *
029400090401     c                   eval      traDFI = Data_Fine
029500120727     c                   eval      da_duplicare =  SI
029600090331     c                   end
029700101105      *
029800120727     c                   If        tipo_aggior = Cambio_PIVA
029900101105     c                   eval      tratur = '7'
030000120727      *
030100120727     c                   elseif    tipo_aggior = Cambio_Societa
030200101105     c                   eval      tratur = '8'
030300101105     c                   end
030400101105      *
030500090331     c                   update    aitra001                             79
030600090401     C   79              move      *on           Errore
030700090401    >C   79              leavesr
030800090331      *
030900090331      *  replica l'autista
031000120727     c                   If        da_duplicare =  SI
031100090331     c                   z-add     NUMERO_nrc    traNRC
031200090401     c                   eval      traDIN = Data_Inizio
031300090331     c                   eval      traDFI = 0
031400120731     c                   clear                   TRATur
031500120731     c                   clear                   TRAfiller
031600090401      *
031700120727     c                   if        tipo_aggior = Cambio_Societa
031800090401     c                   eval      trasocG= Societa
031900090506      *
032000120727     c                   elseIf    tipo_aggior = Cambio_PIVA
032100120903      *
032200120903      * 3/9/2012 dopo telefonata di chiarimento:
032300120903      * =======================================
032400120903      *  Giovanni Taglialatela NON ha mai utilizzato questa funzione xchè i casi
032500120903      *   sono rari e soprattutto le filiali non sono mai completamente allineate
032600120903      *   su tutti gli autisti nello stesso momento.
032700120903      *  In ogni caso, non ha sotto mano i dati degli automezzi in possesso
032800120903      *  delle filiali e, comunque, x maggior sicurezza, è bene che le informazioni
032900120903      *  sugli automezzi siano reinserite dalla Filiale stessa a fronte di un
033000120903      *  cambio di questo tipo....onde evitare di riportare dati NON aggiornati.
033100120903      *    QUINDI si è scelto di ripulire tutte le informazioni relative agli
033200120903      *    automezzi.
033300120903      *
033400090506      *  deve eliminare tutti i dati del Mezzo
033500120903     c                   clear                   TRATIA
033600120903     c                   clear                   TRATAA
033700120903     c                   clear                   TRABPO
033800120903     c                   clear                   TRAANT
033900120903     c                   clear                   TRATMP
034000120903     c                   clear                   TRADTM
034100120903     c                   clear                   TRADASS
034200120903     c                   clear                   TRAPUB
034300120903     c                   clear                   TRAFUR
034400120903     c                   clear                   TRASPI
034500120903     c                   clear                   TRANT1
034600120903     c                   clear                   TRANT2
034700120903     c                   clear                   TRADT2
034800120903     c                   clear                   TRADT3
034900120903     c                   clear                   TRADT4
035000120903     c                   clear                   TRADRR
035100120903     c                   clear                   TRADPR
035200120903     c                   clear                   TRADP2
035300120903     c                   clear                   TRADP3
035400120903     c                   clear                   TRADP4
035500120903     c                   clear                   TRANRP
035600120903     c                   clear                   TRAFF1
035700120903     c                   clear                   TRAFF2
035800120903     c                   clear                   TRARIF
035900160111     c                   clear                   TRAloci
036000160111     c                   clear                   TRAdipa
036100160111     c                   clear                   TRAitaest
036200160111     c                   clear                   TRAfiller1
036300120731      *
036400120731     c                   end
036500110506      *
036600110506      *  Per il Cambio di Società deve controllare se le Anagrafiche siano state
036700110506      *   adeguate correttamente.
036800120727     c                   if        tipo_aggior = Cambio_Societa
036900110506      * Aggancia Anagrafica
037000110506     c                   if        traKAU  > 0
037100110506     c                   eval      tipo = 'A'
037200110506     c                   move      traKAU        autista
037300110506     c     kAUT          chain     fiapd01l
037400110506     c                   if        %Found(fiapd01l) and APDATB <> *blank
037500110506     c                               or
037600110506     c                             NOT %Found(fiapd01l)
037700110506     c                   exsr      piu_in_KAU
037800110506     c                   else
037900110506     c                   movel     apdcsf        RCOSOCIETA
038000110506     c                   move      *all'0'       RCOKSC
038100110506     c                   move      apdksc        RCOKSC
038200110506     C                   move      'F'           subnatura
038300110506      * - controllare la congruenza fra le PIVA
038400110506      * dal fornitore di autofatturazione preso dal codice autista FIAPD00F.
038500110506     C     Krco98j       chain     anRCO98j
038600110506      * confronta la P.IVA del fornitore con quella immessa e
038700110506      * se diversa segnala l'errore
038800110506     c                   if        SOGPARTIVA <> trsIVA and %Found(anRCO98j)
038900110506     c                               or
039000110506     c                             NOT %Found(anRCO98j)
039100110506     c                   exsr      piu_in_KAU
039200110506     c                   endif
039300110506     c                   end
039400110506     c                   endIF
039500090401      *
039600110506      * Aggancia Anagrafica Affl.Defluenze
039700110506     c                   if        traBPT  > 0
039800110506     c                   eval      tipo = 'D'
039900110506     c                   move      traBPT        autista
040000110506     c     kAUT          chain     fiapd01l
040100110506     c                   if        %Found(fiapd01l) and APDATB <> *blank
040200110506     c                               or
040300110506     c                             NOT %Found(fiapd01l)
040400110506     c                   exsr      piu_in_BPT
040500110506     c                   else
040600110506     c                   movel     apdcsf        RCOSOCIETA
040700110506     c                   move      *all'0'       RCOKSC
040800110506     c                   move      apdksc        RCOKSC
040900110506     C                   move      'F'           subnatura
041000110506      * - controllare la congruenza fra le PIVA
041100110506      * dal fornitore di autofatturazione preso dal codice autista FIAPD00F.
041200110506     C     Krco98j       chain     anRCO98j
041300110506      * confronta la P.IVA del fornitore con quella immessa e
041400110506      * se diversa segnala l'errore
041500110506     c                   if        SOGPARTIVA <> trsIVA and %Found(anRCO98j)
041600110506     c                               or
041700110506     c                             NOT %Found(anRCO98j)
041800110506     c                   exsr      piu_in_BPT
041900110506     c                   endif
042000110506     c                   end
042100110506     c                   endIF
042200110506      *
042300090331      *  decodifica società tramite la PIVA nuova
042400090401     c                   exsr      Decod_RAG_IVA
042500090401     c                   if        S1_Trovato <> *Off
042600090401      * in immissione non ci deve essere la stessa P.IVA x la società Filiale
042700090401     c                   eval      traSOC = S1_RagSocKSC
042800090401     C                   End
042900090401      *
043000120727     c                   if        da_segnalare =  NO
043100110509     c                   eval      trafil = Filiale
043200090331     c                   write     aitra000                             79
043300090401     C   79              move      *on           Errore
043400090401    >C   79              leavesr
043500110506     c                   end
043600090331     c                   end
043700090331      *
043800110506     c                   end
043900120727      *
044000120727     c                   endIF
044100120727      *
044200090401     c     SAVnrc        reade     aitra01l
044300090331     c                   endDO
044400090331      *
044500090331     C                   ENDSR
044600090331     C************************************************************
044700090331     C* Chiude il contratto
044800090331     C************************************************************
044900090331     C     DataFineC_Db2 BEGSR
045000090331     C*
045100090331     C* Dalla Window 3 imposta la data di chiusura contratto
045200090401     C     NRec_TRS      chain     aitrs00f
045300090421     c                   eval      trsDFC = Data_Fine
045400090331     c                   update    aitrs000                             79
045500090331     C* se rcd non scrivibile attivo errore
045600090401     C   79              move      *on           Errore
045700090401    >C   79              leavesr
045800090507      *************
045900090331      *  Deve annullare tutti i record collegati di AITRA a quel contratto
046000090507      ******************************
046100090507      **   NON deve più FARLO ****** Deve essere fatto a mano prima aut x aut
046200090507      ******************************
046300090507     c**** TRSnrc        setll     aitra01l
046400090507     c**** TRSnrc        reade     aitra01l
046500090507      *************
046600090507     c*************      Dow       not %EoF(aitra01l)
046700090507      *************
046800090331      *  Solo se non era annullato o già disaccreditato
046900090507     c*************      if        traDFI = 0 and traANN = *blank
047000090507     c*************      eval      traDFI = Data_Fine
047100090507     c*************      end
047200090507      *************
047300090507     c*************      update    aitra001                             79
047400090507     C***79 *******      move      *on           Errore
047500090507    >C***79 *******      leavesr
047600090507      *************
047700090507     c**** TRSnrc        reade     aitra01l
047800090507     c*************      endDO
047900090507      *************
048000090331      *
048100090331     C                   ENDSR
048200090401     C/EJECT
048300090401     c**********************************************************************
048400090401     c* reperimento numero contratto
048500090401     c**********************************************************************
048600090401     C     repnum        BEGSR
048700090401     c                   clear                   numero_NRC        7 0
048800090401     c                   clear                   trul33ds
048900090401     c                   move      'L'           i33tla
049000090401     c                   z-add     32            i33cnu
049100090401     c                   z-add     1             i33num
049200090401     c                   movel     trul33ds      kpjbu
049300090401     c                   call      'TRUL33R'
049400090401     c                   PARM                    kpjba
049500090401     c                   movel     kpjbu         trul33ds
049600090401     c                   if        o33err <> *zeros
049700090401     c                   dump
049800090401     c                   else
049900090401     c                   z-add     o33nrf        numero_NRC
050000090401     c                   endif
050100090401     C                   ENDSR
050200090401     c*--------------------------------------------------------------
050300090401     c* tramite Società e Unità decodifica P.IVA su PROJ
050400090401     c*--------------------------------------------------------------
050500090401     C     Decod_Rag_IVA BegSR
050600090401      **
050700090401      **  Routine x Reperire dati Fornitore:
050800090401      **    La routine in base alla sottonatura può funzionare
050900090401      **     x F=Fornitore/C=Cliente
051000090408     c                   clear                   trmz70s1ds
051100090401     C                   movel(p)  TRSiva        S1_PartitaIVA                     Input
051200090408     C                   movel(p)  'F'           S1_SottoNatur                     Input "F/C"
051300090401     C                   movel(p)  TRSSOCG       S1_Societa                        Input/Output
051400090401     C                   movel(p)  TRSFIL        S1_Unita                          Input/Output
051500090401     c                   call      'TRMZ70SR1'
051600090408     C                   PARM                    trmz70s1ds
051700090408      *
051800090401     C                   ENDSR
051900090401     C/EJECT
052000090401     C************************************************************
052100110506     C************************************************************
052200110506     C* GESTIONE LISTA
052300110506     C************************************************************
052400110506     C     GESS1         BEGSR
052500110506     C*
052600110506     C* inizializzazione videata
052700110506     C     $INZS1        IFEQ      *ON
052800110506     C                   EXSR      INZS1
052900110506     C                   MOVE      *OFF          $INZS1
053000110506     C                   ENDIF
053100110506     C*
053200110506     C* emissione piede videata
053300110506     C                   WRITE     Z1
053400110506     C* Non ci sono records
053500110506     C     WMAX          IFEQ      0
053600110506     C                   WRITE     D1
053700110506     c                   setoff                                       30
053800110506     C                   Else
053900110506     c                   seton                                        30
054000110506      *
054100110506      * se si è effettuato un Posizionamento
054200110506     C                   Z-ADD     1             C1RCD
054300110506     C                   Z-ADD     1             C1nrr
054400110506     C                   ENDIF
054500110506     C*              *------------------*
054600110506     C                   EXFMT     C1
054700110506     C*              *------------------*
054800110506     C     C1NRR         IFNE      0
054900110506     C                   Z-ADD     C1NRR         WSFL
055000110506     C                   Else
055100110506     C                   Z-ADD     SFLNRR        wsfl
055200110506     C                   ENDIF
055300110506      *
055400110506     C                   Z-ADD     SFLNRR        C1RCD
055500110506     C* Selezioni
0556001105061    C                   SELECT
055700110506     C* F12=Ritorno
055800110509     C     $TASTO        WHENEQ    F06
055900110509     C                   EXSR      F06S1
0560001105061O   C                   OTHER
056100110506     C* CONTROLLO DATI
056200110506     C                   EXSR      CTRC1
056300110506     C     *IN99         IFEQ      *OFF
056400110506     C                   EXSR      CTRS1
056500110506     C                   END
0566001105061-   C                   ENDSL
056700110506     C*
056800110506     C                   ENDSR
056900110506     C/EJECT
057000110506     C************************************************************
057100110506     C* INIZIALIZZAZIONE LISTA
057200110506     C************************************************************
057300110506     C     INZC1         BEGSR
057400110506     C* pulizia SFL
057500110506     C                   SETOFF                                         3031
057600110506     C                   WRITE     C1
057700110506     C                   SETON                                          31
057800110506     C*
057900110506     C* CARICAMENTO SFL totale
058000110506     C                   Z-ADD     1             C1RCD
058100110506     c                   movel     DSPGM         c1pgm
058200110506     c                   movel     knmus         c1mus
058300110506     c                   movel     knsif         c1sif
058400110506     C*
058500110509     c                   eval      c1tes1 = 'ATTENZIONE: NON sono stati accredi'
058600110509     c                             + 'tati perchè, sulle relative anagrafiche,'
058700110509     c                   eval      c1tes2 = 'il Fornitore NON corrisponde alla '
058800110509     c                             + 'Società a cui dovrebbero essere legati.'
058900110509      *
059000110509      *  Stampa Testata
059100110509     C                   Except    TESERR
059200110509      *
059300110509      *
059400110506     C                   ENDSR
059500110506     C/EJECT
059600110506     C************************************************************
059700110506     C* INIZIALIZZAZIONE LISTA
059800110506     C************************************************************
059900110506     C     INZS1         BEGSR
060000110506     C*
060100110506     C* pulizia SFL
060200110506    >C                   EXSR      INZC1
060300110506     C*
060400110506     C                   Z-ADD     0             S1NRR
060500110506     C                   Z-ADD     0             WMAX
060600110506     C                   Z-ADD     0             wsfl
060700110506     C*
060800110506     c                   z-add     0             cicla             3 0
060900110506    >C                   EXSR      REDANA
061000110506     C* Carico il SFL
061100110506     C                   EXSR      ROLS1
061200110506     C*
061300110506     C                   Z-ADD     1             WPAG
061400110506     C*
061500110506     C                   ENDSR
061600110506     C************************************************************
061700110506     C* CARICAMENTO PAGINA LISTA
061800110506     C************************************************************
061900110506     C     ROLS1         BEGSR
062000110509     C*
062100110509     c                   clear                   cod1              7 0
062200110509     c                   clear                   cod2              7 0
062300110509     c                   clear                   des1             35
062400110509     c                   clear                   des2             35
062500110506     C*
062600110506     C                   SETOFF                                       32
062700110506     C                   Z-ADD     0             Y
062800110506     C                   Z-ADD     WMAX          S1NRR
062900110506     C*
063000110506     C* Leggo dal file anagrafico per caricare la lista
0631001105061    C     $EFILE        DOWEQ     *OFF
063200110506     C*
063300110506     c                   eval      s1des   = campo72
063400110506     C*
063500110506     C                   ADD       1             S1NRR
063600110506     C                   EVAL      RrnLast = S1nrr
063700110506     C                   ADD       1             Y
063800110506     C*
063900110509     C                   eval      cod1 = KAU_segnalare(y)
064000110509     C                   eval      cod2 = BPT_segnalare(y)
064100110509     C                   eval      des1 = KAU_RagSoc(y)
064200110509     C                   eval      des2 = BPT_RagSoc(y)
064300110509     C*
064400110509     C                   Except    DETERR
064500110506     C                   WRITE     S1
064600110506     C*
064700110506     C* esce x il max. records consentiti è 100
064800110506     c                   if        s1nrr = LIMITE
064900110506     c                   leave
065000110506     c                   end
065100110506     C*
065200110506    >C                   EXSR      REDANA
065300110506     C*
0654001105061-   C                   ENDDO
065500110506     C*
065600110506     C                   Z-ADD     S1NRR         WMAX                 30
065700110506     C*----------
065800110506     C*
065900110506     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
066000110506     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
066100110506     C                   MVR                     RESTO             3 0
066200110506     C     PAGINE        MULT      SFLPAG        C1RCD
0663001105061    C     RESTO         IFGT      0
066400110506     C                   ADD       1             C1RCD
0665001105061E   C                   ELSE
066600110506     C                   SUB       SFLPAG        C1RCD
066700110506     C                   ADD       1             C1RCD
0668001105061-   C                   ENDIF
066900110506     C*
067000110506     C                   ENDSR
067100110506     C************************************************************
067200110506     C* LETTURA RCD ARCHIVIO PILOTA
067300110506     C************************************************************
067400110506     C     REDANA        BEGSR
067500110506     C*
067600110506     C                   MOVEL     *OFF          $EFILE
067700110506     C*
0678001105061    C     $EFILE        DOUEQ     *ON
067900110506      *
068000110506      *     LEGGE la Skiera
068100110506     C                   MOVEL     *ON           $RCDOK
068200110506     c                   add       1             cicla
068300110506      *
068400110506     C*  fine schiera oppure fine codici
068500110506     c                   if        cicla = LIMITE or
068600110506     c                             (
068700110506     C                              Kau_segnalare(cicla) = 0
068800110506     C                               and
068900110506     C                              Bpt_segnalare(cicla) = 0
069000110506     c                             )
069100110506     C*
069200110506     C                   MOVEL     *ON           $EFILE
069300110506     C                   MOVEL     *OFF          $RCDOK
069400110506     C                   MOVE      $EFILE        *IN33
069500110506     c                   end
069600110506      *
069700110506     c                   clear                   campo72          72
069800110506      *
069900110506     C* se il record è giusto esce dal ciclo e compone il campo da emettere
070000110506     C     $RCDOK        ifeq      *On
070100110506     c                   eval      campo72 =
070200110506     c                               %editc(Kau_segnalare(cicla):'Z') + ' '
070300110506     c                             + %subst(Kau_Ragsoc(cicla):1:27) + '  | '
070400110506     c                             + %editc(Bpt_segnalare(cicla):'Z') + ' '
070500110506     c                             + %subst(Bpt_Ragsoc(cicla):1:27)
070600110506     c                   Leave
070700110506     c                   end
070800110506     C*
0709001105061-   C                   ENDDO
071000110506     C*
071100110506     C                   ENDSR
071200110506     C************************************************************
071300110506     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
071400110506     C************************************************************
071500110506     C     CLCPAG        BEGSR
071600110506     C* Input :
071700110506     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
071800110506     C*          cursore
071900110506     C* - SFLPAG = numero rcd per pagina sfl
072000110506     C* Output :
072100110506     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
072200110506     C*
072300110506     C     WSFL          DIV       SFLPAG        PAGINE            4 0
072400110506     C                   MVR                     RESTO             3 0
072500110506     C     RESTO         IFGT      0
072600110506     C                   ADD       1             PAGINE
072700110506     C                   ENDIF
072800110506     C     PAGINE        MULT      SFLPAG        WPAG
072900110506     C*
073000110506     C                   ENDSR
073100110506     C************************************************************
073200110506     C* GESTIONE F12 VIDEO S1
073300110506     C************************************************************
073400110509     C     F06S1         BEGSR
073500110506     C*
073600110509     C                   TIME                    TIMES             6 0
073700110509     C*
073800110509     C*
073900110506     C                   MOVE      *ON           $FINE
074000110506     C* fine programma
074100110506     C                   ENDSR
074200110506     C/EJECT
074300110506     C************************************************************
074400110506     C* CONTROLLO TESTATA LISTA
074500110506     C************************************************************
074600110506     C     CTRC1         BEGSR
074700110506     C*
074800110506     C                   MOVE      *OFF          *IN99
074900110506      *
075000110506     C                   ENDSR
075100110506     C************************************************************
075200110506     C*
075300110506     C************************************************************
075400110506     C     CTRS1         BEGSR
075500110506     C*
075600110506     C                   MOVEL     *OFF          $ESCI
075700110506     C                   SETOFF                                       99
075800110506     C*
075900110506     C* Leggo il sfl solo se ci sono rcd
0760001105061    C     WMAX          IFGT      0
076100110506     C                   READC     S1                                     21
076200110506     C*
076300110506     C* esce se fine sfl o errore che richiede l'uscita
0764001105062    C     *IN21         DOWEQ     *OFF
076500110506     C     $ESCI         ANDEQ     *OFF
076600110506     C                   Z-ADD     S1NRR         C1RCD
076700110506     C* ctrl su riga
076800110506     C                   EXSR      RECS1
076900110506      *
077000110506     c                   setoff                                       32
077100110506     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0772001105063    C     *IN99         IFEQ      *ON
077300110506     C     $ESCI         OREQ      *ON
077400110506     C                   MOVE      *ON           *IN32
077500110506     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
077600110506     C* la ripristinerò a conclusione del ciclo di READC
077700110506     C                   MOVE      *OFF          $INZS1
0778001105063-   C                   ENDIF
077900110506     C*
078000110506     C                   UPDATE    S1
078100110506     C*
078200110506     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0783001105063    C     $ESCI         IFEQ      *OFF
078400110506     C                   READC     S1                                     21
078500110506     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0786001105064    C     *IN21         IFEQ      *ON
078700110506     C                   MOVE      WINZS1        $INZS1
078800110506     C* calcolo pagina a cui deve posizionarsi
078900110506     C                   EXSR      CLCPAG
0790001105064-   C                   ENDIF
0791001105063-   C                   ENDIF
079200110506     C*
0793001105062-   C                   ENDDO
079400110506     C*
0795001105061-   C                   ENDIF
079600110506     C*
079700110506     C                   ENDSR
079800110506     C/EJECT
079900110506     C************************************************************
080000110506     C* aggiunge a schiera segnalazioni AUTISTI città
080100110506     C************************************************************
080200110506     C     Piu_in_KAU    BEGSR
080300110506     C*
080400120727     c                   eval             da_segnalare =  SI
080500120727     c                   eval      Codici_da_segnalare =  SI
080600110506      *  invece deve essere segnalato
080700110506      *   che no è stato scritto il record del cliente
080800110506     c                   add       1             NrKAU
080900110506     c                   eval      KAU_Segnalare(NrKAU) = traKAU
081000110506     c                   eval      KAU_RagSoc(NrKAU) = apdrsf
081100110506      *
081200110506     C                   ENDSR
081300110506     C/EJECT
081400110506     C************************************************************
081500110506     C* aggiunge a schiera segnalazioni AUTISTI Affl/Defl
081600110506     C************************************************************
081700110506     C     Piu_in_BPT    BEGSR
081800110506     C*
081900120727     c                   eval             da_segnalare =  SI
082000120727     c                   eval      Codici_da_segnalare =  SI
082100110506      *  invece deve essere segnalato
082200110506      *   che no è stato scritto il record del cliente
082300110506     c                   add       1             NrBPT
082400110506     c                   eval      BPT_Segnalare(NrBPT) = traBPT
082500110506     c                   eval      BPT_RagSoc(NrBPT) = apdrsf
082600110506      *
082700110506     C                   ENDSR
082800110506     C/EJECT
082900110506     C************************************************************
083000110506     C* CONTROLLO CAMPI I/O RIGA LISTA
083100110506     C************************************************************
083200110506     C     RECS1         BEGSR
083300110506     C*
083400110506     C* reset indicatori DSPATR
083500110506     C                   MOVE      *ALL'0'       IN4049           10
083600110506     C                   MOVEA     IN4049        *IN(40)
083700110506     C*
083800110506     C                   ENDSR
083900110506     C/EJECT
084000110506     C************************************************************
084100110506     C* OPERAZIONI INIZIALI
084200110506     C************************************************************
084300110506     C     *INZSR        BEGSR
084400110506     C*
084500110506     C* Variabili per gestione videate
084600110506     C                   MOVE      *BLANK        $GEST             2
084700110506     C                   MOVE      *BLANK        $FINE             1
084800110506     C                   MOVE      *BLANK        $INZS1            1
084900110506     C                   MOVE      *BLANK        $EFILE            1
085000110506     C                   MOVE      *BLANK        $ESCI             1
085100110506     C                   MOVE      *BLANK        $RCDOK            1
085200110506     C* Indici
085300110506     C                   Z-ADD     0             X                 3 0
085400110506     C                   Z-ADD     0             Y                 3 0
085500110506     C*
085600110506     C* Reperimento tasti di funzione
085700110506
085800110506     C*  Impostazioni di inizio programma
085900110506     C*
086000110506     C                   ENDSR
086100110506     C************************************************************
086200110509     O*  STAMPA ERRORI !!!!!!!!
086300110509      *--------------------------------------------------------------*
086400110509     OSYSPRT    E            TESERR           01
086500110509     O                       STT(1)              66
086600110509     O                       STT(2)             132
086700110509     O                       KNMUS               20
086800110509     O                       KNSIF               32
086900110509     O                       UDATE              118 '  /  /  '
087000110509     O                       PAGE          Z    132
087100110509      *
087200110509     O          E            TESERR           03
087300110509     O                       TIMES              118 '  .  .  '
087400110509      *
087500110509     O          E            TESERR           06
087600110509     O                                           24 'AUTISTI  DI  CITTA''    '
087700110509     O                                           68 'AFFLUENZE/DEFLUENZE'
087800110509      *
087900110509     O          E            TESERR      0  2
088000110509     O                                           24 'AUTISTI  DI  CITTA''    '
088100110509     O                                           68 'AFFLUENZE/DEFLUENZE'
088200110509      *
088300110509     O          E            DETERR         1
088400110509     o                       COD1          Z     07
088500110509     o                       DES1               + 1
088600110509     o                       COD2          Z     57
088700110509     o                       DES2               + 1
088800110509      *
088900110509      *--------------------------------------------------------------*
089000110509** STT
089100110509                                    ***  L I S T A     A U T I S T
089200110509 I   N O N    RIACCREDITATI  ***   TRMZ70SR3             PAG.
