000100001116     H DECEDIT('0,') DATEDIT(*DMY.)
000200001030     H* TNVR03R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400001025     H*  DATI RICEVUTI IN FILIALE DA "FNTASC0R"                      *
000500000000     H*--------------------------------------------------------------*
000600001026     FFNARB01L  IF   E           K DISK
000700001027     FTABEL00F  IF   E           K DISK    USROPN
000800001027     FAZORG01L  IF   E           K DISK    USROPN
000900001027     FFNTASC0R  UF   E           K DISK    USROPN
001000001025     FCMRIC01L  UF   E           K DISK
001100001027     FFNLBL01L  IF   E           K DISK
001200001027     FFNLBL02L  IF   E           K DISK
001300001027     F                                     RENAME(FNLBL000:FNLBLMA)
001400001121     FFNtRBC0T  O    E             DISK    USROPN
001500001027     F                                     RENAME(FNARBC00:FNARBCT)
001600001121     FFNtRBC46  O    E             DISK    USROPN
001700001027     F                                     RENAME(FNARBC00:FNARBCS)
001800950411     D*--------------------------------------------------------------*
001900950411     D*    S C H I E R E                                             *
002000950411     D*--------------------------------------------------------------*
002100001027     D* SCHIERE X TRASMISSIONI
002200001027     D FIL             S              3  0 DIM(500)                             FILIALE
002300001027     D FEL             S              3  0 DIM(500)                             FILIALE ELABORAT.
002400001027     D TFE             S              3  0 DIM(500)                             TERMINAL LNP
002500001030     D* CODICI BOLLE DA NON INVIARE IN SEDE
002600001030     D CDB             S              2    DIM(50)                              COD. BOLLE
002700001027     D*--------------------------------------------------------------*
002800930422     D CMD1            S             55    DIM(2) CTDATA PERRCD(1)
002900930422     D CMD2            S             55    DIM(2) CTDATA PERRCD(1)
003000930422     D CMD3            S             55    DIM(2) CTDATA PERRCD(1)
003100001025     D CMD4            S             55    DIM(2) CTDATA PERRCD(1)
003200001027     D* SCHIERE X QCMDEXEC
003300001027     D CMDC            S             45    DIM(2) CTDATA PERRCD(1)              QCAEXEC CHKOBJ
003400001027     D CMDA            S             45    DIM(2) CTDATA PERRCD(1)              QCAEXEC ADDPFM
003500001027     D CMDO            S             45    DIM(2) CTDATA PERRCD(1)              QCAEXEC OVRDBF
003600001027     D CMDL            S             40    DIM(1) CTDATA PERRCD(1)              QCAEXEC ALCOBJ
003700001027     D CMDD            S             40    DIM(1) CTDATA PERRCD(1)              QCAEXEC DLCOBJ
003800001025      *--------------------------------------------------------------*
003900001027     D W236            S            236
004000001027     D §Fil            S              3    INZ(' ')
004100001027     D Cancella        S              1    INZ('N')
004200001027     D Open46          S              1    INZ('N')
004300001027     D OpenC           S              1    INZ('N')
004400001027     D VAR             S              4    INZ(' ')
004500001027     D VAR1            S              5    INZ(' ')
004600001027     D VAR2            S              4    INZ(' ')
004700001027     D VAR11           S             11    INZ(' ')
004800001027     D VAR12           S             12    INZ(' ')
004900001027     D Comman          S             80    INZ(' ')
005000001027     D WAlloc          S              1    INZ('N')
005100001027     D LIBR            S             10    INZ(' ')
005200001027     D FILE            S             10    INZ(' ')
005300001027     D MBR             S             10    INZ(' ')
005400001027     D VARMBR          S              4    INZ(' ')
005500001027      *--------------------------------------------------------------*
005600001027     D NrrT            S             10  0 INZ(0)
005700001027     D NrrC            S             10  0 INZ(0)
005800001027     D Lung            S             15  5 INZ(0)
005900001027     D NuRec           S              7  0 INZ(0)
006000001027     D Volte           S              3  0 INZ(0)
006100001027     D SavLnp          S              3  0 INZ(0)
006200001027     D §FLE            S              3  0 INZ(0)
006300001027     D ORA             S              6  0 INZ(0)
006400001027     D Time            S              6  0 INZ(0)
006500001027     D Hm              S              4  0 INZ(0)
006600001027     D Data6           S              6  0 INZ(0)
006700001027     D WhhUDT          S             14  0 INZ(0)
006800001027     D AmgUDT          S              8  0 INZ(0)
006900001027     D GmaUDT          S              8  0 INZ(0)
007000001027     D X               S              3  0 INZ(0)
007100001027     D H               S              2  0 INZ(0)
007200001027      *--------------------------------------------------------------*
007300000000     D KPJBA         E DS
007400900604     D  LIBSYS                92    101
007500900604     D  LEGFIL               483    492
007600900604     D  MBRFIL               493    502
007700001027      *-----------------------------
007800930517     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007900001027     D  TCU                  398    697
008000001027     D                                     DIM(50)                              PARAM FLG7/8,TIPC
008100001027     D  KCU                  698    847P 0
008200001027     D                                     DIM(50)                              PARAM CAPI CONTO
008300001027     D                                     PACKEVEN
008400001027     D  DTU                  848    895P 0
008500001027     D                                     DIM(12)                              DATE PARAM
008600001027     D                                     PACKEVEN
008700001027      *-----------------------------
008800941025     D                SDS
008900941025     D  §PGM                   1     10
009000001027      *-----------------------------
009100001030     D DS3A          E DS
009200001030     D DS7A          E DS
009300001027     D DSCMZ         E DS
009400930517     D CNCR80        E DS
009500001027     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
009600001027     D TIBS50        E DS                  EXTNAME(TIBS50DS)
009700001027     D FNLV55        E DS                  EXTNAME(FNLV55DS)
009800001027      *-----------------------------
009900020503     D*                DS
010000020503     D* §SEI                   1      6
010100020503     D* §PRF                   1      3
010200001027      *-----------------------------
010300941025     D WLBDA8          DS
010400941025     D  G08DAT                 1      8  0
010500941025     D  G08INV                 9     16  0
010600941025     D  G08ERR                17     17
010700941025     D  G08TGI                18     22  0
010800001025      *---------------------------------------------------------------*
010900001027      * Ciclo Principale:
011000001027      *     lettura del membro ricevuto -> FNTASC0R
011100001026      *---------------------------------------------------------------*
011200001027     C                   EXSR      Open_TAS
011300001025      *
011400001027     C* Ripete il giro 2 volte
011500001027     C*   il primo per individuare le bolle singole o figlie
011600001027     C*   il secondo per individuare quelle originali
011700001027     C                   DO        2             Volte
011800001027     C*
011900001027     C     *Loval        Setll     FNTASC0R
012000001027     C                   READ      FNTASC0R                               01
012100001025      *
012200001025     C     *IN01         DOWEQ     *OFF
012300001025      *
012400001027      * Legge BOLLE x Check dal file SPIA
012500001027     C     Volte         Ifeq      1
012600001026     C                   EXSR      Dettaglio
012700001027     C                   else
012800001030     C                   EXSR      Originale
012900001027     C                   end
013000001025      *
013100001027     C     Cancella      Ifeq      'S'
013200001027     C                   ADD       1             NuRec
013300001030     C                   DELETE    FNTASC0R
013400001027     C                   EndIf
013500001025      *
013600001025     C                   READ      FNTASC0R                               01
013700001025     C                   ENDDO
013800911016     C*
013900001027     C                   ENDDO
014000001027     C*
014100001027     C* Chiude Membro FNTASC0R
014200001027     C                   EXSR      Close_TAS
014300930422     C*
014400001027     C*  Esegue le trasmissioni
014500001109     c                   goto      fine
014600001109      *
014700001027     C                   EXSR      Trasmiss
014800001027      *
014900001025     C     FINE          TAG
015000001027     C                   Exsr      Chiude_Pgm
015100000000     C                   SETON                                        LR
015200001026     C**------------------------------------------------------------
015300001026     C** Esegue i controlli di dettaglio
015400001026     C**------------------------------------------------------------
015500001026     C     Dettaglio     BEGSR
015600001026     C*
015700001026     C* 1) se esiste FNARB con data consegna (0) deve eliminare semplicemente
015800001027     C*   il record spia.
015900001026     C* 2) se esiste FNARB con data consegna ed è già stato trasmesso in sede
016000001026     C*  deve scrivere un FNARBC0T con i dati della bolla e trasmetterlo in
016100001026     C*   sede e alla filiale di partenza come il giro sul FNLR90R.
016200001026     C* 3) se FNARB non c'è più occorre preparare un FNARBC0T da trasmettere
016300001026     C*  in sede e alla filiale di partenza e i dati di consegna devono essere
016400001026     C*   presi dal file ricevuto da sede e/o forzati.
016500001026     C*
016600001026     C     KARB          Chain     FNARB01L
016700001026     C*
016800001026     C* se c'è il record in FNARB
016900001026     C                   If        %Found(FNARB01L)
017000001026      *
017100001030      *   non è stato ancora consegnato quindi cancello il record spia
017200001026     C                   If        ArbDCM = 0
017300001027     C                   eval      Cancella = 'S'
017400001026     C                   Else
017500001030      *
017600001030      *   Solo se ho una data consegna ed ho trasmesso il record
017700001228     C                   If        ArbFT2 = 'T'  or
017800001228     C                             ArbFT1 <>'T' and ArbFT2 <>'T' and ArbNMI > 0
017900001229     C                             and ArbNMI < 9999999 and ArbDMI < amgUDT
018000001026     C                   Exsr      FNARB_Si
018100001102     C                   Else
018200001102     C                   eval      Cancella = 'S'
018300001102     C                   Endif
018400001027      *
018500001026     C                   End
018600001026      *
018700001026     C                   Else
018800001026     C                   Exsr      FNARB_No
018900001027     C                   eval      Cancella = 'S'
019000001026     C                   End
019100001026     C*
019200001026     C                   ENDSR
019300001026     C**------------------------------------------------------------
019400001026     C** Esegue i controlli di dettaglio se trovato FNARB
019500001026     C**------------------------------------------------------------
019600001026     C     FNARB_Si      BEGSR
019700001030      *
019800001030      *  Imposta la linea x controllo rottura di livello
019900001030     C                   movel     TssLnp        LNPsav
020000001027     C                   Exsr      Open_File
020100001030      *
020200001030      *  Controllo eventuali dirottamenti bolle figlie etc.
020300001030      ** Esegue i controlli di dettaglio se ha trovato FNARB
020400001027      *
020500001030      * Quando entra qui la Data Consegna Merce è <>(0)
020600001030      *   quindi
020700001030      *    se spedizione CONSEGNATA CALCOLO TEMPI DELIVERY
020800001030     C                   MOVE      *ZEROS        ARBNCI
020900001030     C                   MOVE      *ZEROS        ARBNPI
021000001030     C                   MOVE      *ZEROS        ARBDCE
021100001030     C                   MOVE      *ZEROS        ARBNRC
021200001030      *
021300001030      * Guardo cosa devo TRASMETTERE alla PARTENZA se ci sono legami
021400001030     C                   MOVEL     *BLANK        HaFIGL            1
021500001030     C                   MOVEL     *BLANK        HaMAMM            1
021600001030     C                   MOVEL     *BLANK        HaORIG            1
021700001030     C                   MOVEL     *BLANK        ARBFAG
021800001030      * >>>>>>>>>
021900001030      *   La Bolla ha una FIGLIA con LNA diversa (DIROTT./RESO)
022000001030     C     KARB          SETGT     FNLBL02L
022100001030     C     KARB          READPE    FNLBL02L                               22
022200001030      *
022300001030     C     *IN22         DOWEQ     *off
022400001030     C     ARBLNA        IFNE      LBLLAN
022500001030     C                   MOVEL     'S'           HaFIGL
022600001030     C                   END
022700001030     C     KARB          READE     FNLBL02L                               22
022800001030     C                   ENDDO
022900001030      * >>>>>>>>>
023000001030      *   La bolla ha una MAMMA + guardo LNA bolla ORIGINALE
023100001030     C     KARB          CHAIN     FNLBL01L                           22
023200001030      *
023300001030     C     *IN22         IFEQ      *Off
023400001030     C                   MOVEL     'S'           HaMAMM
023500001030     C     LBLLAO        IFNE      LBLLAP
023600001030     C     LBLLAO        ORNE      LBLLAN
023700001030     C                   MOVEL     'S'           HaORIG
023800001030     C                   END
023900001030     C                   ENDIF
024000001030      *
024100001030      * se ha figlia altra FIL.ma non ha MAMMA è la 1°
024200001030      *  quindi non trasmetto DATA CONSEGNA
024300001030     C     HaFIGL        IFEQ      'S'
024400001030     C     HaMAMM        ANDEQ     ' '
024500001030     C     ARBCCA        ANDNE     '2'
024600001030     C     ARBCCA        ANDNE     '6'
024700001030     C                   MOVEL     'D'           ARBFAG                         BOLLA DIROTTATA
024800001030     C                   END
024900001030      *-----------------------------------------------------*
025000001030      * Calcolo TEMPI Delivery
025100001030     C                   MOVE      ARBAAS        D98AAS
025200001030     C                   MOVE      ARBLNP        D98LNP
025300001030     C                   MOVE      ARBNRS        D98NRS
025400001030     C                   MOVE      ARBNSP        D98NSP
025500001030     C                   CALL      'TNSD99R'
025600001030     C                   PARM                    TNSD99
025700001228     C                   Z-ADD     D98NCI        ARBNCI                         NON CONF.CONS. INT.
025800001228     C                   Z-ADD     D98NPI        ARBNPI                         NON CONF.PART. INT.
025900001228     C                   Z-ADD     D98DCE        ARBDCE                         DATA CONS. PREV.CLI.
026000001228     C                   Z-ADD     D98NRC        ARBNRC                         NON CONF. RESA CLI.
026100001030      *-----------------------------------------------------*
026200001030     C                   MOVEL     ' '           ARBFDS
026300001030      * Alla filiale
026400001030     C                   add       1             NrrT
026500001030     C                   write     FnARBCt
026600001030      *
026700001030      * A sede solo se deve essere inviata
026800001030     C                   z-add     1             §F
026900001030     C     ArbCBO        lookup    CDB(§F)                                03
027000001030     C     *in03         Ifeq      *Off
027100001030     C                   add       1             NrrC
027200001030     C                   write     FnARBCs
027300001030     C                   end
027400001030      *
027500001030      * Ha BOLLA ORIG.con Linea Arrivo diversa e non ha figlia con
027600001030      * LNA diversa : è l'ultima bolla della catena quindi devo trasm.
027700001030      * alla BOLLA ORIGIN. la data consegna.
027800001030      *   Non cancella il record per RILEGGERLO SUCCESSIVAMENTE.
027900001030     C     HaORIG        IFEQ      'S'
028000001030     C     HaFIGL        ANDEQ     ' '
028100001030     C                   move      'N'           Cancella
028200001030      *
028300001030     c                   move      ARBNCI        TSSNCI
028400001030     c                   move      ARBNPI        TSSNPI
028500001030     c                   move      ARBDCE        TSSDCE
028600001030     c                   move      ARBNRC        TSSNRC
028700001030     C                   update    FNTASC00
028800001030      *
028900001030     C                   ELSE
029000001030     C                   move      'S'           Cancella
029100001030     C                   ENDIF
029200001030      *
029300001030     C                   ENDSR
029400001027      **-------------------------------------------------------------**
029500001030      ** Solo nel secondo giro
029600001027      **-------------------------------------------------------------**
029700001030     C     Originale     BEGSR
029800001030      *
029900001030      * Con il secondo giro cerca la bolla originale
030000001030     C     KARB          CHAIN     FNLBL01L                           22
030100001030      *
030200001030      *  Imposta la linea x controllo rottura di livello
030300001030      *    con la Linea di Partenza Bolla Originale
030400001030     C                   movel     LblLpo        LNPsav
030500001030     C                   Exsr      Open_File
030600001030      *
030700001030     C                   MOVE      LblLPO        ARBLNP
030800001030     C                   MOVE      LblAAO        ARBAAS
030900001030     C                   MOVE      LblNRO        ARBNRS
031000001030     C                   MOVE      LblNSO        ARBNSP
031100001030     C                   MOVEL     ' '           ARBFDS                         SI DETT.SEGN.
031200001030     C                   MOVEL     'O'           ARBFAG                         BOLLA ORIGINALE
031300001030      *
031400001030     C                   add       1             NrrT
031500001030     C                   write     FnARBCt
031600001030      *
031700001030      * Se non trasmetto a sede la Bolla FIGLIA non trasmetto
031800001030      * neanche la Bolla ORIGINALE
031900001030      *
032000001030      * A sede solo se deve essere inviata
032100001030     C                   z-add     1             §F
032200001030     C     ArbCBO        lookup    CDB(§F)                                03
032300001030     C     *in03         Ifeq      *Off
032400001030     C                   add       1             NrrC
032500001030     C                   write     FnARBCs
032600001030     C                   end
032700001030      *
032800001030      * Elimina il record
032900001030     C                   move      'S'           Cancella
033000001030      *
033100001030     C                   ENDSR
033200001030      **------------------------------------------------------------
033300001030      ** Esegue i controlli di dettaglio se non trovato FNARB
033400001030      **------------------------------------------------------------
033500001030     C     FNARB_No      BEGSR
033600001030      *
033700001030      *  Imposta la linea x controllo rottura di livello
033800001030     C                   movel     TssLnp        LNPsav
033900001030     C                   Exsr      Open_File
034000001030      *
034100001030      *  Prepara come può i dati da inviare sia in sede che alla filiale
034200001030      *  di partenza.
034300001030     C                   eval      ARBLNP = TSSLNP
034400001030     C                   eval      ARBAAS = TSSAAS
034500001030     C                   eval      ARBNRS = TSSNRS
034600001030     C                   eval      ARBNSP = TSSNSP
034700001030     C                   eval      ARBDTI = TSSDTI
034800001030     C                   eval      ARBHTI = TSSHTI
034900001229     C                   eval      ARBDCE = TSSDCE
035000001229     C                   eval      ARBDCM = TSSDCE
035100001120     C                   eval      ARBNPI = 0
035200001120     C                   eval      ARBNCI = 0
035300001120     C                   eval      ARBNRC = 0
035400001030     C                   eval      ARBDPC = TSSDPC
035500001030     C                   eval      ARBDUC = TSSDUC
035600001030     C                   eval      ARBFLP = TSSFLP
035700001030     C                   eval      ARBDET = TSSDET
035800001030     C                   eval      ARBDUT = TSSDUT
035900001030     C                   eval      ARBDAM = TSSDAM
036000001030     C                   eval      ARBNDC = TSSNDC
036100001030     C                   eval      ARBDDC = TSSDDC
036200001030     C                   eval      ARBDCP = TSSDCP
036300001030     C                   eval      ARBFDS = TSSFDS
036400001030     C                   eval      ARBHMC = TSSHMC
036500001030     C                   eval      ARBCCA = TSSCCA
036600001030     C                   eval      ARBTMC = TSSTMC
036700001030     C                   eval      ARBFAG = TSSFAG
036800001030      *
036900001030      *  scrive sia x la sede che x la filiale di partenza
037000001030     C                   add       1             NrrT
037100001030     C                   write     FnARBCt
037200001030      *
037300001030     C                   add       1             NrrC
037400001030     C                   write     FnARBCs
037500001030      *
037600001030     C                   ENDSR
037700001030      **-------------------------------------------------------------**
037800001030      ** apertura controllata dei files
037900001030      **-------------------------------------------------------------**
038000001030     C     Open_File     BEGSR
038100001030      *
038200001027      * Apre il file per trasmissione verso la sede una sola volta
038300001027     C                   If        Open46 = 'N'
038400001027     C                   eval      Open46 = 'S'
038500001027     C                   Exsr      Open_46
038600001027     C                   End
038700001027      *
038800001027      * Controllo se la LNP = me stesso o una mia 2° LIVELLO
038900001027     C                   Z-ADD     1             X
039000001030     C     LNPsav        LOOKUP    FIL(X)                                 19
039100001027      * se trovato
039200001027      * Apertura membro a cambio LNP e trasmissione quello precedente
039300001027     C*
039400001027     C   19TFE(X)        IFNE      SAVLNP
039500001102     C*
039600001102     C* lo chiude solo se era prima aperto
039700001121     C                   CLOSE     FNtRBC0T                             99
039800001102     C*  quindi apre nuovamente
039900001027     C                   EXSR      Open_ARBC
040000001121     C                   OPEN      FNtRBC0T
040100001027     C                   Z-ADD     TFE(X)        SAVLNP
040200001027     C                   ENDif
040300001026     C*
040400001026     C                   ENDSR
040500001027     C**-------------------------------------------------------------**
040600001027     C** QCAEXEC SUL MBR DI FNARBC46 PER LA SEDE
040700001027     C**-------------------------------------------------------------**
040800001027     C     Open_46       BEGSR
040900001027     C*
041000001027     C** FILIALE PER APERTURA MEMBRO
041100001027     C                   MOVE      ')'           VAR
041200001027     C                   MOVEL     '046'         VAR
041300001027     C                   MOVE      '))'          VAR1
041400001027     C                   MOVEL     '046'         VAR1
041500001027     C                   Z-ADD     45            LUNG
041600001027     C*
041700001027     C** CONTROLLO SE ESISTE IL MEMBRO
041800001027     C                   SETOFF                                       60
041900001027     C                   MOVE      VAR           CMDC(2)
042000001027     C                   MOVEL     *BLANKS       COMMAN
042100001027     C                   MOVEL     CMDC(2)       COMMAN
042200001027     C                   CALL      'QCMDEXC'                            60
042300001027     C                   PARM                    COMMAN
042400001027     C                   PARM                    LUNG
042500001027     C*
042600001027     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
042700001027     C   60              DO
042800001027     C                   MOVE      VAR           CMDA(2)
042900001027     C                   MOVEL     *BLANKS       COMMAN
043000001027     C                   MOVEL     CMDA(2)       COMMAN
043100001027     C                   CALL      'QCMDEXC'
043200001027     C                   PARM                    COMMAN
043300001027     C                   PARM                    LUNG
043400001027     C                   END
043500001027     C*
043600001027     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
043700001027     C                   MOVE      VAR           CMDO(2)
043800001027     C                   MOVEL     *BLANKS       COMMAN
043900001027     C                   MOVEL     CMDO(2)       COMMAN
044000001027     C                   CALL      'QCMDEXC'
044100001027     C                   PARM                    COMMAN
044200001027     C                   PARM                    LUNG
044300001027     C*
044400001027     C* Provo ad allocare FNARBC46 X 10 volte
044500001027     C                   MOVEL     'N'           WALLOC
044600001027     C                   Z-ADD     1             X
044700001027     C                   Z-ADD     40            LUNG
044800001027     C                   MOVE      VAR1          CMDL(1)
044900001027     C                   MOVEL     *BLANKS       COMMAN
045000001027     C                   MOVEL     CMDL(1)       COMMAN
045100001027     C     WALLOC        DOWEQ     'N'
045200001027     C     X             ANDLE     10
045300001027     C                   CALL      'QCMDEXC'                            60
045400001027     C                   PARM                    COMMAN
045500001027     C                   PARM                    LUNG
045600001027     C  N60              MOVEL     'S'           WALLOC                         Allocazione O.K.
045700001027     C                   ADD       1             X
045800001027     C                   END
045900001027     C*
046000001027     C*  Se non sono riuscita ad allocarlo vado a fine pgm. e
046100001027     C*  risottometto in coda la trasmissione date di consegna
046200001027     C     WALLOC        IFEQ      'N'
046300001109     C*********          MOVEL     '????'        KCOAZ
046400001109     C*********          CALL      'BCH10'
046500001109     C*********          PARM                    KPJBA
046600001027     C                   GOTO      FINE
046700001027     C                   END
046800001027     C*
046900001121     C                   OPEN      FNtRBC46
047000001027     C                   ENDSR
047100001027     C**-------------------------------------------------------------**
047200001027     C** QCAEXEC SUL MBR DI FNARBC0T
047300001027     C**-------------------------------------------------------------**
047400001027     C     Open_ARBC     BEGSR
047500001027      *
047600001027     C** FILIALE PER APERTURA MEMBRO
047700001027     C                   MOVE      ')'           VAR
047800001027     C                   MOVEL     TFE(X)        VAR
047900001027     C                   Z-ADD     45            LUNG
048000001027      *
048100001027     C** CONTROLLO SE ESISTE IL MEMBRO X FNARBC0T/R
048200001027     C                   SETOFF                                       60
048300001027     C                   MOVE      VAR           CMDC(1)
048400001027     C                   MOVEL     *BLANKS       COMMAN
048500001027     C                   MOVEL     CMDC(1)       COMMAN
048600001027     C                   CALL      'QCMDEXC'                            60
048700001027     C                   PARM                    COMMAN
048800001027     C                   PARM                    LUNG
048900001027     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
049000001027     C   60              DO
049100001027     C                   MOVE      VAR           CMDA(1)
049200001027     C                   MOVEL     *BLANKS       COMMAN
049300001027     C                   MOVEL     CMDA(1)       COMMAN
049400001027     C                   CALL      'QCMDEXC'
049500001027     C                   PARM                    COMMAN
049600001027     C                   PARM                    LUNG
049700001027     C                   END
049800001027     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
049900001027     C                   MOVE      VAR           CMDO(1)
050000001027     C                   MOVEL     *BLANKS       COMMAN
050100001027     C                   MOVEL     CMDO(1)       COMMAN
050200001027     C                   CALL      'QCMDEXC'
050300001027     C                   PARM                    COMMAN
050400001027     C                   PARM                    LUNG
050500001027      *
050600001027     C                   ENDSR
050700001027      *----------------------------------------------------------------
050800001027      * PULIZIA PARAMETRI X TRUL51C
050900001027      *----------------------------------------------------------------
051000001027     C     TRUL51        BEGSR
051100001027      *
051200001027     C                   MOVEL     '*LIBL'       LIBR
051300001027     C                   MOVEL     *BLANK        FILE
051400001027     C                   MOVEL     *BLANK        MBR
051500001027     C                   Z-ADD     0             NrrT
051600001027      *
051700001027     C                   ENDSR
051800001027     C**---------------------------------------------------------------
051900001027     C** LANCIO AZIONI DI TRASMISSIONE IN SEDE
052000001027     C**---------------------------------------------------------------
052100001027     C     INSede        BEGSR
052200001027      *
052300001027     C                   EXSR      TRUL51
052400001027      *
052500001121     C                   MOVEL     'FNtRBC46'    FILE
052600001027     C                   MOVEL     'M'           VAR2
052700001027     C                   MOVE      '046'         VAR2
052800001027     C                   MOVEL     VAR2          MBR
052900001027     C                   CALL      'TRUL51C'
053000001027     C                   PARM                    LIBR
053100001027     C                   PARM                    FILE
053200001027     C                   PARM                    MBR
053300001027     C                   PARM                    NrrT
053400001027      *
053500001027     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
053600001027     C* da/a cui spedire
053700001027     C     NrrT          IFGT      0
053800001027     C     NrrT          ANDNE     9999999999
053900001027     C                   CLEAR                   TIBS50
054000001027     C                   MOVEL     *BLANKS       I50TLA
054100001027     C                   MOVEL     KNSIF         I50PSI
054200001027     C                   MOVEL     SIMFEL        I50PPO
054300001027     C                   MOVEL     '046'         I50APO
054400001027     C                   CALL      'TIBS50R'
054500001027     C                   PARM                    TIBS50
054600001027     C* INVIO
054700001109     C*************      EXSR      X25SED
054800001027      *
054900001027     C                   END
055000001027      *
055100001027     C                   ENDSR
055200001027     C**---------------------------------------------------------------
055300001027     C** SOTTOMETTE TRASMISSIONE X LA SEDE
055400001027     C**---------------------------------------------------------------
055500001027     C     X25SED        BEGSR
055600001027     C*
055700001027     C                   CLEAR                   W236
055800001027     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
055900001121     C                   MOVEL     'FNtRBC46'    CMZFLD                          DA: >>>
056000001027     C                   MOVE      VAR2          VAR
056100001027     C                   MOVEL     *BLANK        CMZMBD
056200001027     C                   MOVEL     VAR           CMZMBD
056300001027     C                   MOVEL     O50PLA        CMZLBD
056400001027     C                   MOVEL     'FNARBC0R'    CMZFLA                          A: >>>
056500001027     C                   MOVEL     *BLANK        CMZMBA
056600001027     C                   MOVEL     'M   '        VAR
056700001027     C                   MOVE      SIMFEL        VAR
056800001027     C                   MOVEL     VAR           CMZMBA
056900001027     C                   MOVEL     O50ALA        CMZLBA
057000001027     C*
057100001027     C     H             IFLT      24
057200001027     C                   MOVE      'N'           CMZURG                          NORMALE
057300001027     C                   MOVEL     H             CMZORI
057400001027     C                   Z-ADD     AMGUDT        DATA6                           DATA
057500001027     C                   MOVE      DATA6         CMZDTI                          DATA
057600001027     C                   ELSE
057700001027     C                   MOVE      'N'           CMZURG                          IN NOTTE
057800001027     C                   END
057900001027     C*
058000001027     C                   MOVE      'A'           CMZADD                          IN ADD
058100001027     C                   MOVE      'S'           CMZCLR                          CLEAR
058200001027     C                   MOVE      'S'           CMZFTR                          DA TRANS.
058300001027     C                   MOVE      *BLANK        CMZERR                          1= ERR.
058400001027     C                   MOVE      'TR91'        CMZAZN                          AZIONE
058500001027     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
058600001027     C                   MOVE      NrrT          CMZNRR                          N.REC.
058700001027     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
058800001109      *
058900001109     c                   goto      salta
059000001109      *
059100001027     C                   CALL      'TIBS52C'
059200001027     C                   PARM                    O50LCL
059300001027     C                   PARM                    CMZDST
059400001027     C                   PARM                    CMZFLD
059500001027     C                   PARM                    CMZMBD
059600001027     C                   PARM                    CMZLBD
059700001027     C                   PARM                    CMZFLA
059800001027     C                   PARM                    CMZMBA
059900001027     C                   PARM                    CMZLBA
060000001027     C                   PARM                    CMZORI
060100001027     C                   PARM                    CMZDTI
060200001027     C                   PARM                    CMZURG
060300001027     C                   PARM                    CMZADD
060400001027     C                   PARM                    CMZCLR
060500001027     C                   PARM                    CMZFTR
060600001027     C                   PARM                    CMZERR
060700001027     C                   PARM                    CMZAZN
060800001027     C                   PARM                    CMZSYF
060900001027     C                   PARM                    CMZNRR
061000001027     C                   PARM                    CMZPRR
061100001027     C                   PARM                    W236
061200001027     C*
061300001109     C     salta         tag
061400001109     C*
061500001027     C                   ENDSR
061600001027     C**---------------------------------------------------------------
061700001027     C** LANCIO AZIONI DI TRASMISSIONE
061800001027     C**---------------------------------------------------------------
061900001027     C     INVIO         BEGSR
062000001027     C*
062100001027     C*  IMPOSTO DATI X TRASMISSIONI LOCALI
062200001027     C                   MOVEL     'M'           MBRFIL
062300001027     C                   MOVE      SIMFEL        MBRFIL
062400001027     C                   MOVEL     MBRFIL        LEGFIL
062500001027     C                   MOVEL     'F'           LEGFIL
062600001027     C                   MOVE      '999999'      LEGFIL
062700001027     C*  TRASMETTO
062800001027     C                   MOVEL     'M'           VARMBR
062900001027     C                   MOVE      *ZEROS        X
063000001027     C*
063100001027     C     *IN45         DOUEQ     '1'
063200001027     C                   ADD       1             X
063300001027     C     FIL(X)        COMP      0                                      45
063400001027     C*
063500001027     C  N45FIL(X)        IFEQ      TFE(X)
063600001027     C                   MOVE      TFE(X)        VARMBR
063700001027      *   TESTATA BOLLA
063800001027     C                   EXSR      TRUL51
063900001027      *
064000001121     C                   MOVEL     'FNtRBC0T'    FILE
064100001027     C                   MOVEL     VARMBR        MBR
064200001027     C                   CALL      'TRUL51C'
064300001027     C                   PARM                    LIBR
064400001027     C                   PARM                    FILE
064500001027     C                   PARM                    MBR
064600001027     C                   PARM                    NrrT
064700001027      *
064800001027     C                   Z-ADD     NrrT          NrrC
064900001027      *
065000001027      * RICHIAMO ROUTINE PER REPERIMENTO LIBR. E S.INFORMATIVI
065100001027      * A CUI TRASMETTERE
065200001027     C     NrrC          IFGT      0
065300001027     C     NrrC          ANDNE     9999999999
065400001027      *
065500001027     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
065600001027     C* da/a cui spedire
065700001027     C                   CLEAR                   TIBS50
065800001027     C                   MOVEL     *BLANKS       I50TLA
065900001027     C                   MOVEL     KNSIF         I50PSI
066000001027     C                   MOVEL     SIMFEL        I50PPO
066100001027     C                   MOVEL     TFE(X)        I50APO
066200001027     C                   CALL      'TIBS50R'
066300001027     C                   PARM                    TIBS50
066400001027      *   ALTRA FILIALE   = TRASMISSIONE
066500001027     C                   MOVE      FEL(X)        §FIL
066600001109      *
066700001109     C***********        EXSR      X25
066800001109      *
066900001027     C                   END
067000001027      *
067100001027     C                   END
067200001027     C                   END
067300001027      *
067400001027     C                   ENDSR
067500001027     C**---------------------------------------------------------------
067600001027     C** SOTTOMETTE TRASMISSIONE  TESTATA BOLLA
067700001027     C**---------------------------------------------------------------
067800001027     C     X25           BEGSR
067900001027      *
068000001027     C                   MOVE      §FIL          CMZDST                          DEST.:FIL
068100001121     C                   MOVEL     'FNtRBC0T'    CMZFLD                          DA: >>>
068200001027     C                   MOVEL     *BLANK        CMZMBD
068300001027     C                   MOVEL     VARMBR        CMZMBD
068400001027     C                   MOVEL     O50PLA        CMZLBD
068500001027     C                   MOVEL     'FNARBC0R'    CMZFLA                          A: >>>
068600001027     C                   MOVEL     *BLANK        CMZMBA
068700001027     C                   MOVEL     'M   '        VAR
068800001027     C                   MOVE      SIMFEL        VAR
068900001027     C                   MOVEL     VAR           CMZMBA
069000001027     C                   MOVEL     O50ALA        CMZLBA
069100001027     C                   MOVE      'LR91'        CMZAZN                          AZIONE
069200001027     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
069300001027     C                   MOVE      NrrT          CMZNRR                          N.REC.
069400001027     C*
069500001027     C                   EXSR      XCMZ
069600001027     C*
069700001027     C                   ENDSR
069800001027     C**---------------------------------------------------------------
069900001027     C* imposta il CMZ
070000001027     C**---------------------------------------------------------------
070100001027     C     XCMZ          BEGSR
070200001027     C*
070300001027     C                   MOVE      '0000'        CMZORI                          ORA
070400001027     C                   MOVE      '0000000'     CMZDTI                          DATA
070500001027      *
070600001027      * TEST SE DEVO TRASMETTERE IN IMMED O IN NOTTE
070700001027     C                   TIME                    TIME
070800001027     C                   MOVEL     TIME          HM
070900001027      *
071000001027     C     HM            IFLE      §7AHMD
071100001027     C                   MOVEL     HM            H
071200001027     C                   ADD       1             H
071300001027     C     H             IFLT      24
071400001027     C                   MOVE      'N'           CMZURG                          NORMALE
071500001027     C                   MOVEL     H             CMZORI
071600001027     C                   Z-ADD     AMGUDT        DATA6                           DATA
071700001027     C                   MOVE      DATA6         CMZDTI                          DATA
071800001027     C                   ELSE
071900001027     C                   MOVE      'N'           CMZURG                          IN NOTTE
072000001027     C                   END
072100001027     C                   ELSE
072200001027     C                   MOVE      'N'           CMZURG                          IN NOTTE
072300001027     C                   END
072400001027      *
072500001027     C                   MOVE      'A'           CMZADD                          IN ADD
072600001027     C                   MOVE      'S'           CMZCLR                          CLEAR
072700001027     C                   MOVE      'S'           CMZFTR                          DA TRANS.
072800001027     C                   MOVE      *BLANK        CMZERR                          1= ERR.
072900001109      *
073000001109     c                   goto      salta1
073100001109      *
073200001027     C                   CALL      'TIBS52C'
073300001027     C                   PARM                    O50LCL
073400001027     C                   PARM                    CMZDST
073500001027     C                   PARM                    CMZFLD
073600001027     C                   PARM                    CMZMBD
073700001027     C                   PARM                    CMZLBD
073800001027     C                   PARM                    CMZFLA
073900001027     C                   PARM                    CMZMBA
074000001027     C                   PARM                    CMZLBA
074100001027     C                   PARM                    CMZORI
074200001027     C                   PARM                    CMZDTI
074300001027     C                   PARM                    CMZURG
074400001027     C                   PARM                    CMZADD
074500001027     C                   PARM                    CMZCLR
074600001027     C                   PARM                    CMZFTR
074700001027     C                   PARM                    CMZERR
074800001027     C                   PARM                    CMZAZN
074900001027     C                   PARM                    CMZSYF
075000001027     C                   PARM                    CMZNRR
075100001027     C                   PARM                    CMZPRR
075200001027     C                   PARM                    W236
075300001027      *
075400001109     C     salta1        tag
075500001109      *
075600001027     C                   ENDSR
075700001030      **---------------------------------------------------------
075800001030      ** Esegue le trasmissioni
075900001030      **---------------------------------------------------------
076000001030     C     Trasmiss      BEGSR
076100001030      *
076200001030      * Aggiorna ARCHIVIO di LOG
076300001030     C     LEGFIL        CHAIN     CMRIC01L                           20
076400001030     C  N20              DO
076500001030     C                   Z-ADD     NUREC         RICNR1
076600001030     C                   MOVE      '1'           RICFL2                          AGG.TO
076700001030     C                   UPDATE    CMRIC000
076800001030     C                   END
076900001030      *
077000001030      * I N V I O  FILIALI
077100001030     C                   EXSR      INVIO
077200001030     C*
077300001030     C* I N V I O  A SEDE    se ci sono delle bolle
077400001121     C                   CLOSE     FNtRBC46
077500001030     C     NrrC          IFGT      0
077600001030     C                   EXSR      INSEDE
077700001030     C                   END
077800001030     C*
077900001121      * Disalloco FNtRBC46
078000001030     C                   Z-ADD     40            LUNG
078100001030     C                   MOVE      VAR1          CMDD(1)
078200001030     C                   MOVEL     *BLANKS       COMMAN
078300001030     C                   MOVEL     CMDD(1)       COMMAN
078400001030     C                   CALL      'QCMDEXC'                            60
078500001030     C                   PARM                    COMMAN
078600001030     C                   PARM                    LUNG
078700001030     C*
078800001030     C                   ENDSR
078900001030     C**---------------------------------------------------------
079000001030     C** operazioni di chiusura programma
079100001030     C**---------------------------------------------------------
079200001030     C     Chiude_Pgm    BEGSR
079300001030     C*
079400001030     C* Imposto i dati in BS50
079500001030     C                   CLEAR                   TIBS50
079600001030     C                   MOVEL     'C'           I50TLA
079700001030     C                   CALL      'TIBS50R'
079800001030     C                   PARM                    TIBS50
079900001030     C*
080000001030     C* Imposto i dati in LV55
080100001030     C                   CLEAR                   FNLV55
080200001030     C                   MOVEL     'L'           D55TLA
080300001030     C                   CALL      'FNLV55R'
080400001030     C                   PARM                    FNLV55
080500001030     C*
080600001030     C                   ENDSR
080700001030     C**---------------------------------------------------------
080800001030     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FNTASC0R
080900001030     C**---------------------------------------------------------
081000001030     C     Open_TAS      BEGSR
081100001030     C*
081200001030     C                   MOVE      ')'           VAR11
081300001030     C                   MOVEL     MBRFIL        VAR11
081400001030     C                   MOVE      '))'          VAR12
081500001030     C                   MOVEL     MBRFIL        VAR12
081600001030     C                   Z-ADD     55            LUNG
081700001030     C*
081800001030     C                   SETOFF                                       20
081900001030     C                   MOVE      VAR11         CMD1(1)
082000001030     C                   MOVEL     *BLANKS       COMMAN
082100001030     C                   MOVEA     CMD1(1)       COMMAN
082200001030     C                   CALL      'QCMDEXC'                            20
082300001030     C                   PARM                    COMMAN
082400001030     C                   PARM                    LUNG
082500001030     C   20              GOTO      FINE
082600001030     C*
082700001030     C** CONTROLLO SE ESISTE IL MEMBRO
082800001030     C** SE NON ESISTE CHIUDO IL PROGRAMMA
082900001030     C*
083000001030     C                   SETOFF                                       20
083100001030     C                   MOVE      VAR12         CMD2(1)
083200001030     C                   MOVEL     *BLANKS       COMMAN
083300001030     C                   MOVEA     CMD2(1)       COMMAN
083400001030     C                   CALL      'QCMDEXC'                            20
083500001030     C                   PARM                    COMMAN
083600001030     C                   PARM                    LUNG
083700001030     C   20              GOTO      FINE
083800001030     C*
083900001030     C** ALLOCO MEMBRO
084000001030     C** SE NON RIESCE CHIUDO IL PROGRAMMA
084100001030     C                   SETOFF                                       20
084200001030     C                   MOVE      VAR11         CMD3(1)
084300001030     C                   MOVEL     *BLANKS       COMMAN
084400001030     C                   MOVEA     CMD3(1)       COMMAN
084500001030     C                   CALL      'QCMDEXC'                            20
084600001030     C                   PARM                    COMMAN
084700001030     C                   PARM                    LUNG
084800001030     C   20              GOTO      FINE
084900001030     C*
085000001030     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
085100001030     C** SE NON RIESCE CHIUDO IL PROGRAMMA
085200001030     C                   OPEN      FNTASC0R
085300001030     C*
085400001030     C                   ENDSR
085500001030     C**------------------------------------------------------------
085600001030     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FNTASC0R
085700001030     C**------------------------------------------------------------
085800001030     C     Close_TAS     BEGSR
085900001030     C*
086000001030     C                   MOVE      ')'           VAR11
086100001030     C                   MOVEL     MBRFIL        VAR11
086200001030     C                   MOVE      '))'          VAR12
086300001030     C                   MOVEL     MBRFIL        VAR12
086400001030     C                   Z-ADD     55            LUNG
086500001030     C*
086600001030     C                   CLOSE     FNTASC0R
086700001030     C*
086800001030     C                   MOVE      VAR12         CMD4(1)
086900001030     C                   MOVEL     *BLANKS       COMMAN
087000001030     C                   MOVEA     CMD4(1)       COMMAN
087100001030     C                   CALL      'QCMDEXC'
087200001030     C                   PARM                    COMMAN
087300001030     C                   PARM                    LUNG
087400001030     C*
087500001030     C** DISALLOCA IL MEMBRO AGG.TO
087600001030     C*
087700001030     C                   ENDSR
087800001030     C*---------------------------------------------------------------*
087900001030     C* OPERAZIONI INIZIALI X INDIVIDUARE IL MEMBRO DA LEGGERE
088000001030     C*---------------------------------------------------------------*
088100001030     C     *INZSR        BEGSR
088200001030      *
088300001030     C     *ENTRY        PLIST
088400001030     C                   PARM                    KPJBA
088500001030      *
088600001030     C     *LIKE         DEFINE    tssLNP        LNPsav
088700001030     C     *LIKE         DEFINE    TBLCOD        COD
088800001030     C     *LIKE         DEFINE    TBLKEY        KEY
088900001030     C     *LIKE         DEFINE    tssAAS        KAAS
089000001030     C     *LIKE         DEFINE    tssLNP        KLNP
089100001030     C     *LIKE         DEFINE    tssNRS        KNRS
089200001030     C     *LIKE         DEFINE    tssNSP        KNSP
089300001030      *
089400001030     C     KTAB          KLIST
089500001030     C                   KFLD                    CODUT
089600001030     C                   KFLD                    COD
089700001030     C                   KFLD                    KEY
089800001030      *
089900001030     C     KTAB1         KLIST
090000001030     C                   KFLD                    CODUT
090100001030     C                   KFLD                    COD
090200001030      *
090300001030     C     KARB          KLIST
090400001030     C                   KFLD                    TSSAAS
090500001030     C                   KFLD                    TSSLNP
090600001030     C                   KFLD                    TSSNRS
090700001030     C                   KFLD                    TSSNSP
090800001030      *
090900001030     C                   Z-ADD     1             CODUT
091000001030     C                   CALL      'X§PARUT'
091100001030     C                   PARM                    UTEDSE
091200001030     C     CODUT         CABEQ     -1            FINE
091300001030     C                   MOVEL     RAGUT         RSUT             20
091400001030     C                   MOVEL     REC80         CNCR80
091500001030      *
091600001030     C                   MOVEL     ' '           D98TLA
091700001030     C                   MOVEL     'A'           D98TBO
091800001030      *
091900020503     C*                  MOVEL     KNMUS         §SEI
092000020503     C*    §PRF          COMP      'REM'                                  01
092100020503     C*  01              MOVE      §SEI          §FLE
092200020503     C* N01              DO
092300020503     c     simtpp        ifeq      '2'
092400020503     c     simtpp        oreq      *blanks
092500020503     c                   move      simpou        §fle
092600020503     c                   else
092700001030     C     SIMFEL        COMP      0                                      02
092800001030     C  N02              MOVE      SIMFEL        §FLE
092900030606     C   02              MOVE      SIMPOU        §FLE
093000001030     C                   END
093100001030      *
093200001030     C                   TIME                    WHHUDT
093300001030     C                   MOVEL     WHHUDT        ORA
093400001030     C                   MOVE      WHHUDT        G08DAT
093500001030     C                   MOVE      *ZEROS        G08INV
093600001030     C                   MOVEL     *BLANK        G08ERR
093700001030     C                   CALL      'XSRDA8'
093800001030     C                   PARM                    WLBDA8
093900001030     C                   Z-ADD     G08INV        AMGUDT
094000001030     C                   Z-ADD     G08DAT        GMAUDT
094100001030      *
094200001030      * Prelevo ORARIO MASSIMO x TRASMISSIONE in Immediato
094300001030     C                   OPEN      TABEL00F
094400001030     C                   MOVEL     '7A'          COD
094500001030     C                   MOVEL     *BLANKS       KEY
094600001030     C                   MOVEL     '1'           KEY
094700001030     C     KTAB          CHAIN     TABEL                              03
094800001030     C  N03              MOVEL     TBLUNI        DS7A
094900001030      *
095000001030      * Carico i Codici BOLLE da NON inviare IN SEDE
095100001030     C                   MOVEL     '3A'          COD
095200001030     C                   MOVEL     *BLANK        KEY
095300001030     C                   MOVE      *ZEROS        §F                2 0
095400001030     C     KTAB1         Chain     TABEL                              03
095500001030      *
095600001030     C     *IN03         Doweq     *Off
095700001030     C     TblFLG        IFeq      *BLANK                                        ANNUL.
095800001030     C                   MOVEL     TblUNI        DS3A
095900001030     C     §3ABSD        IFeq      'N'                                           NO TRASM.IN SEDE
096000001030     C                   ADD       1             §F
096100001030     C                   MOVEL     TblKEY        CDB(§F)
096200001030     C                   End
096300001030     C                   End
096400001030     C     KTAB1         READE     TABEL                                  03
096500001030     C                   EndDO
096600001102     C                   CLOSE     TABEL00F
096700001030      *
096800001030     C*MEMORIZZO PER OGNI FILIALE LA FILIALE ELABORATORE
096900001030     C                   OPEN      AZORG01L
097000001030     C                   Z-ADD     0             X
097100001030     C     *LOVAL        SETLL     AZORG01L
097200001030     C                   READ      AZORG01L                               19
097300001030      *
097400001030     C     *IN19         DOweq     *Off
097500001030      *
097600001030     C     ORGFAG        IFEQ      'F'
097700001030     C     ORGFAG        OREQ      'A'
097800001030      *  Reperisco terminal di partenza collegato
097900001030     C                   CLEAR                   FNLV55
098000001030     C* Imposto i dati in LV55
098100001030     C                   MOVEL     'P'           D55TPT
098200001030     C                   MOVEL     AMGUDT        D55DRF
098300001030     C                   MOVEL     ORGFIL        D55LNP
098400001030     C                   MOVEL     ORGFIL        D55LIN
098500001030     C                   CALL      'FNLV55R'
098600001030     C                   PARM                    FNLV55
098700001030     C                   ADD       1             X
098800001030     C                   MOVE      ORGFIL        FIL(X)
098900001030     C                   MOVE      ORGFEL        FEL(X)
099000001030     C                   MOVE      D55TFP        TFE(X)
099100001030     C                   END
099200001030      *
099300001030     C                   READ      AZORG01L                               19
099400001030     C                   ENDdo
099500001030     C*
099600001030     C                   CLOSE     AZORG01L
099700001030     C*
099800001030     C                   ENDSR
099900001027     C**-------------------------------------------------------------**
100000001025**         CMD1
100100001025CHKOBJ OBJ(FNTASC0R) OBJTYPE(*FILE)     MBR(M234567890)
100200001025CHKOBJ OBJ(FNTASC0R) OBJTYPE(*FILE)     MBR(M234567890)
100300900604**         CMD2
100400001025ALCOBJ OBJ((FNTASC0R *FILE *EXCL           M234567890))
100500001025ALCOBJ OBJ((FNTASC0R *FILE *EXCL           M234567890))
100600900604**         CMD3
100700001025OVRDBF FILE(FNTASC0R)                   MBR(M234567890)
100800001025OVRDBF FILE(FNTASC0R)                   MBR(M234567890)
100900001025**         CMD4
101000001025DLCOBJ OBJ((FNTASC0R *FILE *EXCL           M234567890))
101100001025DLCOBJ OBJ((FNTASC0R *FILE *EXCL           M234567890))
101200001027**         CMDC
101300001121CHKOBJ OBJ(FNtRBC0T) OBJTYPE(*FILE) MBR(M000)
101400001121CHKOBJ OBJ(FNtRBC46) OBJTYPE(*FILE) MBR(M000)
101500001027**         CMDA
101600001121ADDPFM FILE(FNtRBC0T)               MBR(M000)
101700001121ADDPFM FILE(FNtRBC46)               MBR(M000)
101800001027**         CMDO
101900001121OVRDBF FILE(FNtRBC0T)               MBR(M000)
102000001121OVRDBF FILE(FNtRBC46)               MBR(M000)
102100001027**         CMDL
102200001121ALCOBJ OBJ((FNtRBC46 *FILE *EXCL  M000))
102300001027**         CMDD
102400001121DLCOBJ OBJ((FNtRBC46 *FILE *EXCL  M000))
