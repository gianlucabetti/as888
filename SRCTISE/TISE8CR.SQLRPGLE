000100141219     /*END
000200040820     H DECEDIT('0,') DATEDIT(*dmy.)
000300050510      **********************************************************************
000400140916     fWFTISE8CF uf a E           K DISK    usropn
000500140916      *
000600140916     FAZORG01L  IF   E           K DISK    usropn
000700140916     FAZCMM01L  IF   E           K DISK    usropn
000800140917     FTABEL00F  IF   E           K DISK    usropn
000900140916     FTNTAM01L  IF   E           K DISK
001000050510      **-----------------------------------------------------------------***
001100040820     d KPJBA         E DS
001200140917-->  D DsPassaggio   e DS                  EXTNAME(TISE8BDS)
001300140917     D TITASds       e DS                  EXTNAME(TITAS00F)
001400140917     D BS10DS        E DS                  EXTNAME(TIBS10DS)
001500050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600110511     D CNCR42        E DS
001700140916     D dSTA01        E DS
001800140918     D og143         E DS
001900040820     D*
002000040820     D WLBDAT          DS
002100040820     D  G02DAT                 1      8  0
002200040820     D  G02INV                 9     16  0
002300040820     D  G02ERR                17     17
002400040820     D  G02TGI                18     22  0
002500040820      *
002600040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002700040826     D  almeno_uno     s              1
002800140916     D*------------------
002900140916     D* DS REPERIMENTO DATI UTENTE
003000140916     D*-------------------
003100140916     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
003200140916     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
003300140916     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
003400140916     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003500140916     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
003600140916     D*------------------
003700140916     D* ORGANIGRAMMA
003800140916     D*------------------
003900140916     D FIL             S              3  0 DIM(500)                             *FILIALI
004000140916     D FILD            S             20    DIM(500)
004100140916     D ARE             S              3  0 DIM(500)                             *AREE
004200140916     D ARED            S             20    DIM(500)
004300140916     D DIV             S              1    DIM(500)                             *DIVISIONI
004400140916     D DIVD            S             20    DIM(500)
004500140916     D*------------------
004600140916     D* COMMERCIALI
004700140916     D*------------------
004800141219     d c_Max_Cmm       c                   const(9000)
004900141219     D AGE             S              7  0 DIM(c_Max_Cmm)                       *CODICI AGENTE
005000141219     D AGU             S              7  0 DIM(c_Max_Cmm)                       *CODICI RAGGRUPPAMEN
005100141219     D DAGE            S             25    DIM(c_Max_Cmm)                       *DECODIFICHE AGENTI
005200140916     D*------------------
005300140917     D* campi definiti da altri
005400140916     D*------------------
005500140917     d fldKSU          S                   like(tasKSC)
005600140917     d fldKSUd         S                   like(acoRAG)
005700140917     d fldKSCd         S                   like(acoRAG)
005800140917     d fldAGE          S                   like(tasKSC)
005900140917     d fldFIL          S                   like(orgFIL)
006000140917     d fldARE          S                   like(orgCAR)
006100140917     d fldDIV          S                   like(orgFL3)
006200140917     d fldCLV          S                   like(clpCLV)
006300140917     d fldAGU          S                   like(clpAGE)
006400140917     d fldAGUd         S                   like(cmmDES)
006500140917     d fldFILd         S             20
006600140917     d fldAREd         S             20
006700140917     d fldDIVd         S             20
006800140917     d fldTdpd         S              1
006900140917     d ktbKUT          S                   like(tblKUT)
007000140917     d ktbCOD          S                   like(tblCOD)
007100140917     d ktbKEY          S                   like(tblKEY)
007200140917      **
007300140916     D*------------------
007400050511     D* Reperimento nome PGM
007500050511     D STATUS         SDS           333
007600050511     D  DSPGM            *PROC
007700050511     D*-------------
007800040820     D digits          C                   CONST('0123456789')
007900050511      * ?------------------------------------------------------------------ */
008000140916      * ? nel Periodo selezionato, si devono totalizzare le spedizioni export
008100140916      * ?      delle sole Linee EEX e solo in Porto Franco
008200140916      * ?  per CLIENTE MITTENTE e NAZIONE DESTINATARIO.
008300140916      *      ? Rilevando se il cliente ha avuto tariffe DPD
008400140916      * ? 1) Totalizzando il nr.di Spedizioni
008500140918      * ? 2) Totalizzando il nr.di Spedizioni monocollo  non superiori di 28 kg.
008600140918      * ? 3) Totalizzando il nr.di Spedizioni monocollo  non superiori di 28 kg.
008700140916      * ?    e senza C.O.D.
008800140916      * ?------------------------------------------------------------------ */
008900140916      **  x ottimizzare l'SQL non fa Ordinamento
009000050511      * ?------------------------------------------------------------------ */
009100050510     C/EXEC SQL
009200050510     C+ Declare A1 Cursor for
009300140916     C+ select * from titas00f a where TASTBL = 'F1'        and
009400140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
009500140916     C+   taslna in
009600140918     C+   (SELECT orgfil FROM azorg00f WHERE substr(orgde3, 11, 3)='EEX')
009700140916     C+ UNION all
009800140916     C+ select * from titas10f a where TASTBL = 'F1'        and
009900140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
010000140916     C+   taslna in
010100140918     C+   (SELECT orgfil FROM azorg00f WHERE substr(orgde3, 11, 3)='EEX')
010200140916     C+ UNION all
010300140917     C+ select * from titasP0f a where TASTBL = 'F1'        and
010400140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
010500140916     C+   taslna in
010600140918     C+   (SELECT orgfil FROM azorg00f WHERE substr(orgde3, 11, 3)='EEX')
010700050510     C/END-EXEC
010800050510      *          apertura cursore
010900050510     C/EXEC SQL
011000050510     C+ OPEN A1
011100050510     C/END-EXEC
011200050510     c                   clear                   almeno_uno
011300050510      **
011400050510     C                   DOU       SqlCod <> 0
011500050510      **         lettura cursore
011600050510     C/EXEC SQL
011700140917     C+ FETCH NEXT FROM A1 INTO :TITASDS
011800050510     C/END-EXEC
011900050510     C                   SELECT
012000050510     **
012100050510     C                   WHEN      SqlCod = 100
012200050510     c* a fine file
012300050510     c                   if        almeno_uno = 'S'
012400140916     c                   exsr      daFare_a_EoF
012500050510     c                   else
012600050510     c                   end
012700050510     c                   leave
012800050510     c*
012900050510     C                   WHEN      SqlCod < 0
013000050510     C                   seton                                        H1
013100050510     c                   goto      fine
013200050510     c*
013300050510     C                   OTHER
013400050510     **
013500050510     c* DETTAGLIO
013600050510     c* riga   dettaglio
013700140916     c                   exsr      ElabRig_det
013800050510     C                   ENDSL
013900050510
014000050510     C                   ENDDO
014100050510     C/EXEC SQL
014200050510     C+ Close A1
014300050510     C/END-EXEC
014400050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014500041007     c     fine          tag
014600020326     C                   SETON                                        LR
014700050511     ?****************************************************************************
014800140916      *?   Elabora Riga File di Work
014900050511     ?****************************************************************************
015000140916     c     ElabRig_det   begsr
015100050510      *
015200050511     c                   move      'S'           almeno_uno
015300050524      *
015400140916      *  Verifica esistenza del Cliente/Nazione Destinatario
015500140916      *    x andare in WRITE o in UPDATE
015600140916      *
015700140916     c     KEYworkF      chain     WFtise8CF
015800140916      *
015900140916     c                   If        %Found(WFtise8CF)
016000140916     c                   exsr      UPD_Workfile
016100140916      *
016200140916     c                   else
016300140916     c                   exsr      WRT_Workfile
016400140916      *
016500140916     c                   end
016600140916      *
016700140916     c                   endsr
016800140916     ?****************************************************************************
016900140916      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
017000140916     ?****************************************************************************
017100140916     c     daFare_a_EoF  begsr
017200140916      *
017300140917     c                   close     wfTISE8CF
017400140917      *
017500140917      *  se richiesto richiamare il programma di stampa
017600140917     c                   if        PASLST = 'S'
017700140917     c                   call      'TISE8CR1'
017800140917     c                   parm                    KPJBA
017900140917     c                   end
018000140917      *
018100140916     c                   endsr
018200050511     ?****************************************************************************
018300050511      *?   INIZIALIZZAZIONE
018400050511     ?****************************************************************************
018500040820     C     *inzsr        BEGSR
018600040820      **
018700140916     C     KEYworkF      KLIST
018800140916     C                   Kfld                    TASksc
018900140916     C                   Kfld                    TASnzd
019000050511      **
019100140916     C* CHIAVE LETTURA TABEL00F - COMPLETA
019200140916     C     keyTAB        KLIST
019300140916     C                   KFLD                    KTBKUT                         *CODICE UTENTE
019400140916     C                   KFLD                    KTBCOD                         *CODICE TABELLA
019500140916     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
019600140916     C*
019700040820     C     *ENTRY        PLIST
019800040820     C                   PARM                    KPJBA
019900050510     c                   movel     kpjbu         DsPassaggio
020000050511      **
020100050511     C                   Z-ADD     1             CODUT
020200050511     C                   CALL      'X§PARUT'
020300050511     C                   PARM                    UT§DSE
020400110511     C                   MOVEL     REC42         CNCR42
020500110511     C                   MOVEL     simRGS        RSUT             20
020600041103      *
020700040820      *  imposta UDATE di elaborazione
020800040820     C                   move      *date         G02DAT
020900040820     C                   MOVE      *ZEROS        G02INV
021000040820     C                   MOVE      *BLANKS       G02ERR
021100040820     C                   CALL      'XSRDA8'
021200040820     C                   PARM                    WLBDAT
021300040820     C                   Z-ADD     G02INV        data_oggi
021400140917     C                   move      G02dat        day               8 0
021500140917      *
021600040820     C                   TIME                    W0140            14 0
021700140917     C                   MOVEl     W0140         ore               6 0
021800140916      **
021900050510     c                   move      PASdti        dadata            8 0
022000050510     c                   move      PASdtf        adata             8 0
022100040823      **
022200050511     c                   seton                                        66
022300040825     c                   clear                   prima_volta       1
022400040826     c                   clear                   almeno_uno
022500140917     C                   MOVEL     DSPGM         NOMPGM           10
022600050511      **
022700050511     C                   move      *zeros        G02DAT
022800050511     C                   MOVE      dadata        G02INV
022900050511     C                   MOVE      '3'           G02ERR
023000050511     C                   CALL      'XSRDA8'
023100050511     C                   PARM                    WLBDAT
023200140917     C                   Z-ADD     G02Dat        PDadata           8 0
023300050511      **
023400050511     C                   move      *zeros        G02DAT
023500050511     C                   MOVE      adata         G02INV
023600050511     C                   MOVE      '3'           G02ERR
023700050511     C                   CALL      'XSRDA8'
023800050511     C                   PARM                    WLBDAT
023900140917     C                   Z-ADD     G02dat        PAdata            8 0
024000140917     c*
024100140917     c                   open      wfTISE8CF
024200140917     c*
024300140917     c                   exsr      CARTAB
024400050510     c*
024500040820     C                   ENDSR
024600140916     ?**************************************************************************
024700140916     C* CARTAB - CARICA I DATI IN TABELLE
024800140916     ?**************************************************************************
024900140916     C     CARTAB        BEGSR
025000140916     C*--------------------
025100140916     C* ORGANIGRAMMA
025200140916     C*--------------------
025300140916     C                   Z-ADD     *ZEROS        F                 4 0
025400140916     c                   open      azORG01l
025500140917     c                   open      tabel00f
025600140917      *
025700140916     C     *loval        SETLL     azORG01L
025800140916     C                   READ      azORG01L
025900140916      *
026000140916DO  1C                   DOW       Not %EoF(azORG01L)
026100140916      *
026200140916IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
026300140916IF  3C     orgFAG        andNE     'V'                                          *FILIALE/AGENZIA
026400140916     C                   ADD       1             F
026500140916      *-  Filiale
026600140916     C                   Z-ADD     orgFIL        FIL(F)
026700140916     C                   MOVEL     orgDES        FILD(F)
026800140916      *-  Area
026900140916     C                   Z-ADD     1             KTBKUT
027000140916     C                   MOVEL     '05'          KTBCOD
027100140916     C                   MOVEL     *BLANKS       KTBKEY
027200140916     C                   MOVEL     orgCAR        KTBKEY
027300140916     C     keyTAB        CHAIN     TABEL00F
027400140916IF  5C                   IF        %Found(TABEL00F)
027500140916     C                   Z-ADD     orgCAR        ARE(F)
027600140916     C                   MOVEL     TBLUNI        ARED(F)
027700140916E   5C                   ENDIF
027800140916      *-  Divisione
027900140916     C                   Z-ADD     1             KTBKUT
028000140916     C                   MOVEL     '17'          KTBCOD
028100140916     C                   MOVEL     *BLANKS       KTBKEY
028200140916     C                   MOVEL     orgFL3        KTBKEY
028300140916     C     keyTAB        CHAIN     TABEL00F
028400140916IF  5C                   IF        %Found(TABEL00F)
028500140916     C                   MOVEL     orgFL3        DIV(F)
028600140916     C                   MOVEL     TBLUNI        DIVD(F)
028700140916E   5C                   ENDIF
028800140916      *-
028900140916E   3C                   ENDIF
029000140916     C                   READ      azORG01L                               99
029100140916E   1C                   ENDDO
029200140917      *
029300140916     c                   close     azORG01l
029400140917     c                   close     tabel00f
029500140916     C*--------------------
029600140916     C* COMMERCIALI e UNIficanti
029700140916     C*--------------------
029800140916     C                   Z-ADD     *ZEROS        C                 4 0
029900140916     c                   open      azCMM01l
030000140917      *
030100140916     C     *loval        SETLL     azCMM01l
030200140916     C                   READ      azCMM01l
030300140916DO  1C                   DOW       not %Eof(azCMM01l)
030400140916IF  2C                   IF        cmmATB = *blank
030500140916     C                   ADD       1             C
030600140916     C                   z-add     cmmCOD        AGE(C)
030700140916     C                   z-add     cmmUNI        AGU(C)
030800140916     C                   MOVEL     cmmDES        DAGE(C)
030900140916E   2C                   ENDIF
031000140916     C                   READ      azCMM01l
031100140916E   1C                   ENDDO
031200140917      *
031300140916     c                   close     azCMM01l
031400140916     C*
031500140916     C                   ENDSR
031600140916      *------------------------------------------------------------------------*
031700140916      * NEWKSC - OPERAZIONI PER NUOVO CLIENTE
031800140916      *------------------------------------------------------------------------*
031900140916     C     NEWKSC        BEGSR
032000140916      *
032100140916      *  pulizia campi di work
032200140916     C                   clear                   fldKSCd
032300140916     C                   clear                   fldKSU
032400140916     C                   clear                   fldKSUd
032500140916     C                   clear                   fldCLV
032600140916     C                   clear                   fldAGE                         *CODICE AGENTE
032700140916     C                   clear                   fldAGU                         *CODICE AGENTE
032800140916     C                   clear                   fldAGUd
032900140916     C                   clear                   fldFILd
033000140916     C                   clear                   fldAREd
033100140916     C                   clear                   fldDIVd
033200140916     C                   clear                   fldFIL
033300140916     C                   clear                   fldARE
033400140916     C                   clear                   fldDIV
033500140916     C                   clear                   fldTdpd
033600140916      *
033700140916      *  Codifica il cliente
033800140916     C                   CLEAR                   BS69DS
033900140916     C                   CLEAR                   CLPDS
034000140916     C                   CLEAR                   ACODS
034100140916     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
034200140917     C                   MOVEL     tasKSC        I69KAC                         *CLIENTE X CNACO
034300140916     C                   CALL      'TIBS69R'
034400140916     C                   PARM                    BS69DS
034500140916     C                   PARM                    ACODS
034600140916     C                   PARM                    INDDS
034700140916     C                   PARM                    CLPDS
034800140916     C                   PARM                    CLSDS
034900140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
035000140916     C                   MOVEL     *ALL'*'       fldKSCD
035100140916X   1C                   ELSE                                                   *NO ERRORE
035200140917     C                   movel     acoRAG        fldKSCD
035300140916E   1C                   ENDIF
035400140916     C* IMPOSTA UNIFICANTE
035500140916     c                   Z-ADD     tasKSC        fldKSU
035600140916      *
035700140916     C                   CLEAR                   BS10DS
035800140917     C                   Z-ADD     data_oggi     D10DRF                         *DATA RIFERIMENTO
035900140916     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
036000140916     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
036100140916     C                   MOVE      tasKSC        D10COD
036200140916     C                   CALL      'TIBS10R'
036300140916     C                   PARM                    BS10DS
036400140916      *
036500140916IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
036600140916     C     D10COP        ANDNE     *ZEROS
036700140916     C                   Z-ADD     D10COP        fldKSU                         *UNIFICANTE
036800140916E   2C                   ENDIF
036900140916      *
037000140916      * REPERISCE IL COMMERCIALE,
037100140916      *  SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
037200140916     C                   CLEAR                   BS69DS
037300140916     C                   CLEAR                   CLPDS
037400140916     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
037500140916     C                   MOVEL     fldKSU        I69KCP                         *CLIENTE X CNCLP
037600140917     C                   MOVEL     fldKSU        I69KAC                         *CLIENTE X CNACO
037700140916     C                   CALL      'TIBS69R'
037800140916     C                   PARM                    BS69DS
037900140916     C                   PARM                    ACODS
038000140916     C                   PARM                    INDDS
038100140916     C                   PARM                    CLPDS
038200140916     C                   PARM                    CLSDS
038300140916      *
038400140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
038500140916     C                   MOVEL     *ALL'*'       fldKSUD
038600140916X   1C                   ELSE                                                   *NO ERRORE
038700140917     C                   movel     acoRAG        fldKSUD
038800140916      *
038900140916      * REPERISCE IL CODICE QUALITA'
039000140916     C                   MOVEL     'C'           fldCLV
039100140916IF  1C     CLPCLV        ifne      *BLANKS
039200140916     C                   MOVEL     CLPCLV        fldCLV
039300140916     c                   end
039400140916E   1C                   ENDIF
039500140916      *
039600140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
039700140916     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
039800140916     C                   MOVEL     fldKSU        NUM3              3 0          *CODICE FILIALE +
039900140916     C                   Z-ADD     *ZEROS        fldAGE                         *AGENTE A ZERO  =
040000140916     C                   MOVEL     NUM3          fldAGE                         *FIL+0000
040100140916X   1C                   ELSE                                                   *NO ERRORE
040200140916     C                   Z-ADD     CLPAGE        fldAGE                         *CODICE AGENTE
040300140916E   1C                   ENDIF
040400140916      *
040500140916      * REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
040600140916     C                   Z-ADD     1             C
040700140916     C                   SETOFF                                           99
040800140916     C     fldAGE        LOOKUP    AGE(C)                                 99
040900140916      *
041000140916IF  1C     *IN99         IFEQ      *OFF
041100140916     C                   Z-ADD     fldAGE        fldAGU                         RAGGRUPP = COD AGENT
041200140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGRUP
041300140916X   1C                   ELSE
041400140916      *
041500140916IF  2C     AGU(C)        IFEQ      *ZEROS
041600140916     C                   Z-ADD     fldAGE        fldAGU
041700140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGRUP
041800140916X   2C                   ELSE
041900140916      *
042000140916      * DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
042100140916     C                   Z-ADD     AGU(C)        fldAGU
042200140916     C                   SETOFF                                           99
042300140918      *
042400140918     C                   Z-ADD     1             C
042500140916     C     fldAGU        LOOKUP    AGE(C)                                 99
042600140916IF  3C     *IN99         IFEQ      *ON
042700140916     C                   MOVEL     DAGE(C)       fldAGUd                        DESCRIZIONE RAGGR.TO
042800140916X   3C                   ELSE
042900140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGR.TO
043000140916E   3C                   ENDIF
043100140916E   2C                   ENDIF
043200140916E   1C                   ENDIF
043300140916      *
043400140916      * REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
043500140916     C                   Z-ADD     1             F
043600140916     C                   MOVEL     fldAGU        fldFIL                         *FIL+XXXX
043700140916     C                   SETOFF                                           99
043800140916     C     fldFIL        LOOKUP    FIL(F)                                 99
043900140916IF  1C     *IN99         IFEQ      *ON
044000140916     C                   MOVEL     FILD(F)       fldFILd
044100140916     C                   Z-ADD     ARE(F)        fldARE
044200140916     C                   MOVEL     ARED(F)       fldAREd
044300140916     C                   MOVEL     DIV(F)        fldDIV
044400140916     C                   MOVEL     DIVD(F)       fldDIVd
044500140916E   1C                   ENDIF
044600140916      *
044700140916      *  Controlla se il cliente ha o ha avuto delle tariffe DPD
044800140916     c     tasKSC        setll     TNTAM01L
044900140916     c     tasKSC        reade     TNTAM01L
045000140916     c                   DoW       not %EoF(TNTAM01L)
045100140916      *
045200140916      * solo tar.Italia x DPD
045300140916     c                   if        tamFIE = 'I'
045400140916     c                   eval        dSTA01 = tamFLO
045500140916     c                   if            §taDPD = 'S'
045600140916     C                   eval            fldTdpd = 'S'
045700140916     c                   leave
045800140916     c                   end
045900140916     c                   end
046000140916      *
046100140916     c     tasKSC        reade     TNTAM01L
046200140916     c                   endDO
046300140916      *
046400140916     C                   ENDSR
046500140916     ?****************************************************************************
046600140916      *?   Scrive      File di Work
046700140916     ?****************************************************************************
046800140916     c     WRT_Workfile  begsr
046900140916      *
047000140916     c                   exsr      NEWKSC
047100140916      *
047200140916     c                   clear                   WFTISE8C
047300140916     c                   eval      SE8CDAL  = dadata
047400140916     c                   eval      SE8CAL   =  adata
047500140916     c                   eval      SE8CNZD  = tasNZD
047600140916     c                   eval      SE8CKSC  = tasKSC
047700140916     c                   eval      SE8CKSCD = fldKSCd
047800140916     c                   eval      SE8CCLV  = fldCLV
047900140916     c                   eval      SE8CKSU  = fldKSU
048000140916     c                   eval      SE8CKSUD = fldKSUd
048100140916      *
048200140916      *   Agente / Agente Unificante
048300140916     c                   eval      SE8CAGE  = fldAGE
048400140916     c                   eval      SE8CAGU  = fldAGU
048500140916     c                   eval      SE8CAGUd = fldAGUd
048600140916      *    relativo ad Agente Unificante
048700140916     c                   eval      SE8CFIL  = fldFIL
048800140916     c                   eval      SE8CFILd = fldFILd
048900140916     c                   eval      SE8CARE  = fldARE
049000140916     c                   eval      SE8CAREd = fldAREd
049100140916     c                   eval      SE8CDIV  = fldDIV
049200140916     c                   eval      SE8CDIVd = fldDIVd
049300140916      *    Se ci sono state tariffe x DPD su cliente
049400140916     c                   eval      SE8CTDPD = fldTdpd
049500140916      *
049600140916     c                   exsr      TOT_Spediz
049700140916      *
049800140916     c                   write     WFTISE8C
049900140916      *
050000140916     c                   endsr
050100140916     ?****************************************************************************
050200140916      *?   Update      File di Work
050300140916     ?****************************************************************************
050400140916     c     UPD_Workfile  begsr
050500140916      *
050600140916     c                   exsr      TOT_Spediz
050700140916      *
050800140916     c                   update    WFTISE8C
050900140916      *
051000140916     c                   endsr
051100140916     ?****************************************************************************
051200140916      *?   Totalizza le spedizioni
051300140916     ?****************************************************************************
051400140916     c     ToT_Spediz    begsr
051500140916      *
051600140916      * aggiunge sempre una spedizione
051700140916     c                   eval      SE8CTOT1 = 1 + SE8CTOT1
051800140916      *
051900140918      * se la spedizione ha monocollo o non supera i 28 kg.di bollettato
052000140916      * aggiunge una spedizione
052100140918     c                   if        tasNCL = 1  and tasPKB <=28
052200140916     c                   eval      SE8CTOT2 = 1 + SE8CTOT2
052300140916     c                   end
052400140916      *
052500140916      * se la spedizione ha un solo collo e rimane entro i 28kg di bollettato
052600140916      *  e non ha COD -->(TASCBO="1")
052700140916      * aggiunge una spedizione
052800140917     c                   if        tasNCL = 1  and tasPKB <=28 and
052900140917     c                             tasCBO = '1'
053000140916     c                   eval      SE8CTOT3 = 1 + SE8CTOT3
053100140916     c                   end
053200140916      *
053300140916     c                   endsr
053400140916     ?****************************************************************************
