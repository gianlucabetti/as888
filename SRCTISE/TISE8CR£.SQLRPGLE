000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300140916     fWFTISE8CF uf a E           K DISK    usropn
000400140916      *
000500140916     FAZORG01L  IF   E           K DISK    usropn
000600140916     FAZCMM01L  IF   E           K DISK    usropn
000700140917     FTABEL00F  IF   E           K DISK    usropn
000800140916     FTNTAM01L  IF   E           K DISK
000900050510      **-----------------------------------------------------------------***
001000040820     d KPJBA         E DS
001100140917-->  D DsPassaggio   e DS                  EXTNAME(TISE8BDS)
001200140917     D TITASds       e DS                  EXTNAME(TITAS00F)
001300140917     D BS10DS        E DS                  EXTNAME(TIBS10DS)
001400050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500110511     D CNCR42        E DS
001600140916     D dSTA01        E DS
001700140918     D og143         E DS
001800040820     D*
001900040820     D WLBDAT          DS
002000040820     D  G02DAT                 1      8  0
002100040820     D  G02INV                 9     16  0
002200040820     D  G02ERR                17     17
002300040820     D  G02TGI                18     22  0
002400040820      *
002500040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002600040826     D  almeno_uno     s              1
002700140916     D*------------------
002800140916     D* DS REPERIMENTO DATI UTENTE
002900140916     D*-------------------
003000140916     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
003100140916     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
003200140916     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
003300140916     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003400140916     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
003500140916     D*------------------
003600140916     D* ORGANIGRAMMA
003700140916     D*------------------
003800140916     D FIL             S              3  0 DIM(500)                             *FILIALI
003900140916     D FILD            S             20    DIM(500)
004000140916     D ARE             S              3  0 DIM(500)                             *AREE
004100140916     D ARED            S             20    DIM(500)
004200140916     D DIV             S              1    DIM(500)                             *DIVISIONI
004300140916     D DIVD            S             20    DIM(500)
004400140916     D*------------------
004500140916     D* COMMERCIALI
004600140916     D*------------------
004700140916     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
004800140916     D AGU             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
004900140916     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
005000140916     D*------------------
005100140917     D* campi definiti da altri
005200140916     D*------------------
005300140917     d fldKSU          S                   like(tasKSC)
005400140917     d fldKSUd         S                   like(acoRAG)
005500140917     d fldKSCd         S                   like(acoRAG)
005600140917     d fldAGE          S                   like(tasKSC)
005700140917     d fldFIL          S                   like(orgFIL)
005800140917     d fldARE          S                   like(orgCAR)
005900140917     d fldDIV          S                   like(orgFL3)
006000140917     d fldCLV          S                   like(clpCLV)
006100140917     d fldAGU          S                   like(clpAGE)
006200140917     d fldAGUd         S                   like(cmmDES)
006300140917     d fldFILd         S             20
006400140917     d fldAREd         S             20
006500140917     d fldDIVd         S             20
006600140917     d fldTdpd         S              1
006700140917     d ktbKUT          S                   like(tblKUT)
006800140917     d ktbCOD          S                   like(tblCOD)
006900140917     d ktbKEY          S                   like(tblKEY)
007000140917      **
007100140916     D*------------------
007200050511     D* Reperimento nome PGM
007300050511     D STATUS         SDS           333
007400050511     D  DSPGM            *PROC
007500050511     D*-------------
007600040820     D digits          C                   CONST('0123456789')
007700050511      * ?------------------------------------------------------------------ */
007800140916      * ? nel Periodo selezionato, si devono totalizzare le spedizioni export
007900140916      * ?      delle sole Linee EEX e solo in Porto Franco
008000140916      * ?  per CLIENTE MITTENTE e NAZIONE DESTINATARIO.
008100140916      *      ? Rilevando se il cliente ha avuto tariffe DPD
008200140916      * ? 1) Totalizzando il nr.di Spedizioni
008300140918      * ? 2) Totalizzando il nr.di Spedizioni monocollo  non superiori di 28 kg.
008400140918      * ? 3) Totalizzando il nr.di Spedizioni monocollo  non superiori di 28 kg.
008500140916      * ?    e senza C.O.D.
008600140916      * ?------------------------------------------------------------------ */
008700140916      **  x ottimizzare l'SQL non fa Ordinamento
008800050511      * ?------------------------------------------------------------------ */
008900050510     C/EXEC SQL
009000050510     C+ Declare A1 Cursor for
009100140916     C+ select * from titas00f a where TASTBL = 'F1'        and
009200140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
009300140916     C+   taslna in
009400140918     C+   (SELECT orgfil FROM azorg00f WHERE substr(orgde3, 11, 3)='EEX')
009500140916     C+ UNION all
009600140916     C+ select * from titas10f a where TASTBL = 'F1'        and
009700140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
009800140916     C+   taslna in
009900140918     C+   (SELECT orgfil FROM azorg00f WHERE substr(orgde3, 11, 3)='EEX')
010000140916     C+ UNION all
010100140917     C+ select * from titasP0f a where TASTBL = 'F1'        and
010200140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
010300140916     C+   taslna in
010400140918     C+   (SELECT orgfil FROM azorg00f WHERE substr(orgde3, 11, 3)='EEX')
010500050510     C/END-EXEC
010600050510      *          apertura cursore
010700050510     C/EXEC SQL
010800050510     C+ OPEN A1
010900050510     C/END-EXEC
011000050510     c                   clear                   almeno_uno
011100050510      **
011200050510     C                   DOU       SqlCod <> 0
011300050510      **         lettura cursore
011400050510     C/EXEC SQL
011500140917     C+ FETCH NEXT FROM A1 INTO :TITASDS
011600050510     C/END-EXEC
011700050510     C                   SELECT
011800050510     **
011900050510     C                   WHEN      SqlCod = 100
012000050510     c* a fine file
012100050510     c                   if        almeno_uno = 'S'
012200140916     c                   exsr      daFare_a_EoF
012300050510     c                   else
012400050510     c                   end
012500050510     c                   leave
012600050510     c*
012700050510     C                   WHEN      SqlCod < 0
012800050510     C                   seton                                        H1
012900050510     c                   goto      fine
013000050510     c*
013100050510     C                   OTHER
013200050510     **
013300050510     c* DETTAGLIO
013400050510     c* riga   dettaglio
013500140916     c                   exsr      ElabRig_det
013600050510     C                   ENDSL
013700050510
013800050510     C                   ENDDO
013900050510     C/EXEC SQL
014000050510     C+ Close A1
014100050510     C/END-EXEC
014200050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014300041007     c     fine          tag
014400020326     C                   SETON                                        LR
014500050511     ?****************************************************************************
014600140916      *?   Elabora Riga File di Work
014700050511     ?****************************************************************************
014800140916     c     ElabRig_det   begsr
014900050510      *
015000050511     c                   move      'S'           almeno_uno
015100050524      *
015200140916      *  Verifica esistenza del Cliente/Nazione Destinatario
015300140916      *    x andare in WRITE o in UPDATE
015400140916      *
015500140916     c     KEYworkF      chain     WFtise8CF
015600140916      *
015700140916     c                   If        %Found(WFtise8CF)
015800140916     c                   exsr      UPD_Workfile
015900140916      *
016000140916     c                   else
016100140916     c                   exsr      WRT_Workfile
016200140916      *
016300140916     c                   end
016400140916      *
016500140916     c                   endsr
016600140916     ?****************************************************************************
016700140916      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
016800140916     ?****************************************************************************
016900140916     c     daFare_a_EoF  begsr
017000140916      *
017100140917     c                   close     wfTISE8CF
017200140917      *
017300140917      *  se richiesto richiamare il programma di stampa
017400140917     c                   if        PASLST = 'S'
017500140917     c                   call      'TISE8CR1'
017600140917     c                   parm                    KPJBA
017700140917     c                   end
017800140917      *
017900140916     c                   endsr
018000050511     ?****************************************************************************
018100050511      *?   INIZIALIZZAZIONE
018200050511     ?****************************************************************************
018300040820     C     *inzsr        BEGSR
018400040820      **
018500140916     C     KEYworkF      KLIST
018600140916     C                   Kfld                    TASksc
018700140916     C                   Kfld                    TASnzd
018800050511      **
018900140916     C* CHIAVE LETTURA TABEL00F - COMPLETA
019000140916     C     keyTAB        KLIST
019100140916     C                   KFLD                    KTBKUT                         *CODICE UTENTE
019200140916     C                   KFLD                    KTBCOD                         *CODICE TABELLA
019300140916     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
019400140916     C*
019500040820     C     *ENTRY        PLIST
019600040820     C                   PARM                    KPJBA
019700050510     c                   movel     kpjbu         DsPassaggio
019800050511      **
019900050511     C                   Z-ADD     1             CODUT
020000050511     C                   CALL      'X§PARUT'
020100050511     C                   PARM                    UT§DSE
020200110511     C                   MOVEL     REC42         CNCR42
020300110511     C                   MOVEL     simRGS        RSUT             20
020400041103      *
020500040820      *  imposta UDATE di elaborazione
020600040820     C                   move      *date         G02DAT
020700040820     C                   MOVE      *ZEROS        G02INV
020800040820     C                   MOVE      *BLANKS       G02ERR
020900040820     C                   CALL      'XSRDA8'
021000040820     C                   PARM                    WLBDAT
021100040820     C                   Z-ADD     G02INV        data_oggi
021200140917     C                   move      G02dat        day               8 0
021300140917      *
021400040820     C                   TIME                    W0140            14 0
021500140917     C                   MOVEl     W0140         ore               6 0
021600140916      **
021700050510     c                   move      PASdti        dadata            8 0
021800050510     c                   move      PASdtf        adata             8 0
021900040823      **
022000050511     c                   seton                                        66
022100040825     c                   clear                   prima_volta       1
022200040826     c                   clear                   almeno_uno
022300140917     C                   MOVEL     DSPGM         NOMPGM           10
022400050511      **
022500050511     C                   move      *zeros        G02DAT
022600050511     C                   MOVE      dadata        G02INV
022700050511     C                   MOVE      '3'           G02ERR
022800050511     C                   CALL      'XSRDA8'
022900050511     C                   PARM                    WLBDAT
023000140917     C                   Z-ADD     G02Dat        PDadata           8 0
023100050511      **
023200050511     C                   move      *zeros        G02DAT
023300050511     C                   MOVE      adata         G02INV
023400050511     C                   MOVE      '3'           G02ERR
023500050511     C                   CALL      'XSRDA8'
023600050511     C                   PARM                    WLBDAT
023700140917     C                   Z-ADD     G02dat        PAdata            8 0
023800140917     c*
023900140917     c                   open      wfTISE8CF
024000140917     c*
024100140917     c                   exsr      CARTAB
024200050510     c*
024300040820     C                   ENDSR
024400140916     ?**************************************************************************
024500140916     C* CARTAB - CARICA I DATI IN TABELLE
024600140916     ?**************************************************************************
024700140916     C     CARTAB        BEGSR
024800140916     C*--------------------
024900140916     C* ORGANIGRAMMA
025000140916     C*--------------------
025100140916     C                   Z-ADD     *ZEROS        F                 4 0
025200140916     c                   open      azORG01l
025300140917     c                   open      tabel00f
025400140917      *
025500140916     C     *loval        SETLL     azORG01L
025600140916     C                   READ      azORG01L
025700140916      *
025800140916DO  1C                   DOW       Not %EoF(azORG01L)
025900140916      *
026000140916IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
026100140916IF  3C     orgFAG        andNE     'V'                                          *FILIALE/AGENZIA
026200140916     C                   ADD       1             F
026300140916      *-  Filiale
026400140916     C                   Z-ADD     orgFIL        FIL(F)
026500140916     C                   MOVEL     orgDES        FILD(F)
026600140916      *-  Area
026700140916     C                   Z-ADD     1             KTBKUT
026800140916     C                   MOVEL     '05'          KTBCOD
026900140916     C                   MOVEL     *BLANKS       KTBKEY
027000140916     C                   MOVEL     orgCAR        KTBKEY
027100140916     C     keyTAB        CHAIN     TABEL00F
027200140916IF  5C                   IF        %Found(TABEL00F)
027300140916     C                   Z-ADD     orgCAR        ARE(F)
027400140916     C                   MOVEL     TBLUNI        ARED(F)
027500140916E   5C                   ENDIF
027600140916      *-  Divisione
027700140916     C                   Z-ADD     1             KTBKUT
027800140916     C                   MOVEL     '17'          KTBCOD
027900140916     C                   MOVEL     *BLANKS       KTBKEY
028000140916     C                   MOVEL     orgFL3        KTBKEY
028100140916     C     keyTAB        CHAIN     TABEL00F
028200140916IF  5C                   IF        %Found(TABEL00F)
028300140916     C                   MOVEL     orgFL3        DIV(F)
028400140916     C                   MOVEL     TBLUNI        DIVD(F)
028500140916E   5C                   ENDIF
028600140916      *-
028700140916E   3C                   ENDIF
028800140916     C                   READ      azORG01L                               99
028900140916E   1C                   ENDDO
029000140917      *
029100140916     c                   close     azORG01l
029200140917     c                   close     tabel00f
029300140916     C*--------------------
029400140916     C* COMMERCIALI e UNIficanti
029500140916     C*--------------------
029600140916     C                   Z-ADD     *ZEROS        C                 4 0
029700140916     c                   open      azCMM01l
029800140917      *
029900140916     C     *loval        SETLL     azCMM01l
030000140916     C                   READ      azCMM01l
030100140916DO  1C                   DOW       not %Eof(azCMM01l)
030200140916IF  2C                   IF        cmmATB = *blank
030300140916     C                   ADD       1             C
030400140916     C                   z-add     cmmCOD        AGE(C)
030500140916     C                   z-add     cmmUNI        AGU(C)
030600140916     C                   MOVEL     cmmDES        DAGE(C)
030700140916E   2C                   ENDIF
030800140916     C                   READ      azCMM01l
030900140916E   1C                   ENDDO
031000140917      *
031100140916     c                   close     azCMM01l
031200140916     C*
031300140916     C                   ENDSR
031400140916      *------------------------------------------------------------------------*
031500140916      * NEWKSC - OPERAZIONI PER NUOVO CLIENTE
031600140916      *------------------------------------------------------------------------*
031700140916     C     NEWKSC        BEGSR
031800140916      *
031900140916      *  pulizia campi di work
032000140916     C                   clear                   fldKSCd
032100140916     C                   clear                   fldKSU
032200140916     C                   clear                   fldKSUd
032300140916     C                   clear                   fldCLV
032400140916     C                   clear                   fldAGE                         *CODICE AGENTE
032500140916     C                   clear                   fldAGU                         *CODICE AGENTE
032600140916     C                   clear                   fldAGUd
032700140916     C                   clear                   fldFILd
032800140916     C                   clear                   fldAREd
032900140916     C                   clear                   fldDIVd
033000140916     C                   clear                   fldFIL
033100140916     C                   clear                   fldARE
033200140916     C                   clear                   fldDIV
033300140916     C                   clear                   fldTdpd
033400140916      *
033500140916      *  Codifica il cliente
033600140916     C                   CLEAR                   BS69DS
033700140916     C                   CLEAR                   CLPDS
033800140916     C                   CLEAR                   ACODS
033900140916     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
034000140917     C                   MOVEL     tasKSC        I69KAC                         *CLIENTE X CNACO
034100140916     C                   CALL      'TIBS69R'
034200140916     C                   PARM                    BS69DS
034300140916     C                   PARM                    ACODS
034400140916     C                   PARM                    INDDS
034500140916     C                   PARM                    CLPDS
034600140916     C                   PARM                    CLSDS
034700140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
034800140916     C                   MOVEL     *ALL'*'       fldKSCD
034900140916X   1C                   ELSE                                                   *NO ERRORE
035000140917     C                   movel     acoRAG        fldKSCD
035100140916E   1C                   ENDIF
035200140916     C* IMPOSTA UNIFICANTE
035300140916     c                   Z-ADD     tasKSC        fldKSU
035400140916      *
035500140916     C                   CLEAR                   BS10DS
035600140917     C                   Z-ADD     data_oggi     D10DRF                         *DATA RIFERIMENTO
035700140916     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
035800140916     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
035900140916     C                   MOVE      tasKSC        D10COD
036000140916     C                   CALL      'TIBS10R'
036100140916     C                   PARM                    BS10DS
036200140916      *
036300140916IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
036400140916     C     D10COP        ANDNE     *ZEROS
036500140916     C                   Z-ADD     D10COP        fldKSU                         *UNIFICANTE
036600140916E   2C                   ENDIF
036700140916      *
036800140916      * REPERISCE IL COMMERCIALE,
036900140916      *  SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
037000140916     C                   CLEAR                   BS69DS
037100140916     C                   CLEAR                   CLPDS
037200140916     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
037300140916     C                   MOVEL     fldKSU        I69KCP                         *CLIENTE X CNCLP
037400140917     C                   MOVEL     fldKSU        I69KAC                         *CLIENTE X CNACO
037500140916     C                   CALL      'TIBS69R'
037600140916     C                   PARM                    BS69DS
037700140916     C                   PARM                    ACODS
037800140916     C                   PARM                    INDDS
037900140916     C                   PARM                    CLPDS
038000140916     C                   PARM                    CLSDS
038100140916      *
038200140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
038300140916     C                   MOVEL     *ALL'*'       fldKSUD
038400140916X   1C                   ELSE                                                   *NO ERRORE
038500140917     C                   movel     acoRAG        fldKSUD
038600140916      *
038700140916      * REPERISCE IL CODICE QUALITA'
038800140916     C                   MOVEL     'C'           fldCLV
038900140916IF  1C     CLPCLV        ifne      *BLANKS
039000140916     C                   MOVEL     CLPCLV        fldCLV
039100140916     c                   end
039200140916E   1C                   ENDIF
039300140916      *
039400140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
039500140916     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
039600140916     C                   MOVEL     fldKSU        NUM3              3 0          *CODICE FILIALE +
039700140916     C                   Z-ADD     *ZEROS        fldAGE                         *AGENTE A ZERO  =
039800140916     C                   MOVEL     NUM3          fldAGE                         *FIL+0000
039900140916X   1C                   ELSE                                                   *NO ERRORE
040000140916     C                   Z-ADD     CLPAGE        fldAGE                         *CODICE AGENTE
040100140916E   1C                   ENDIF
040200140916      *
040300140916      * REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
040400140916     C                   Z-ADD     1             C
040500140916     C                   SETOFF                                           99
040600140916     C     fldAGE        LOOKUP    AGE(C)                                 99
040700140916      *
040800140916IF  1C     *IN99         IFEQ      *OFF
040900140916     C                   Z-ADD     fldAGE        fldAGU                         RAGGRUPP = COD AGENT
041000140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGRUP
041100140916X   1C                   ELSE
041200140916      *
041300140916IF  2C     AGU(C)        IFEQ      *ZEROS
041400140916     C                   Z-ADD     fldAGE        fldAGU
041500140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGRUP
041600140916X   2C                   ELSE
041700140916      *
041800140916      * DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
041900140916     C                   Z-ADD     AGU(C)        fldAGU
042000140916     C                   SETOFF                                           99
042100140918      *
042200140918     C                   Z-ADD     1             C
042300140916     C     fldAGU        LOOKUP    AGE(C)                                 99
042400140916IF  3C     *IN99         IFEQ      *ON
042500140916     C                   MOVEL     DAGE(C)       fldAGUd                        DESCRIZIONE RAGGR.TO
042600140916X   3C                   ELSE
042700140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGR.TO
042800140916E   3C                   ENDIF
042900140916E   2C                   ENDIF
043000140916E   1C                   ENDIF
043100140916      *
043200140916      * REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
043300140916     C                   Z-ADD     1             F
043400140916     C                   MOVEL     fldAGU        fldFIL                         *FIL+XXXX
043500140916     C                   SETOFF                                           99
043600140916     C     fldFIL        LOOKUP    FIL(F)                                 99
043700140916IF  1C     *IN99         IFEQ      *ON
043800140916     C                   MOVEL     FILD(F)       fldFILd
043900140916     C                   Z-ADD     ARE(F)        fldARE
044000140916     C                   MOVEL     ARED(F)       fldAREd
044100140916     C                   MOVEL     DIV(F)        fldDIV
044200140916     C                   MOVEL     DIVD(F)       fldDIVd
044300140916E   1C                   ENDIF
044400140916      *
044500140916      *  Controlla se il cliente ha o ha avuto delle tariffe DPD
044600140916     c     tasKSC        setll     TNTAM01L
044700140916     c     tasKSC        reade     TNTAM01L
044800140916     c                   DoW       not %EoF(TNTAM01L)
044900140916      *
045000140916      * solo tar.Italia x DPD
045100140916     c                   if        tamFIE = 'I'
045200140916     c                   eval        dSTA01 = tamFLO
045300140916     c                   if            §taDPD = 'S'
045400140916     C                   eval            fldTdpd = 'S'
045500140916     c                   leave
045600140916     c                   end
045700140916     c                   end
045800140916      *
045900140916     c     tasKSC        reade     TNTAM01L
046000140916     c                   endDO
046100140916      *
046200140916     C                   ENDSR
046300140916     ?****************************************************************************
046400140916      *?   Scrive      File di Work
046500140916     ?****************************************************************************
046600140916     c     WRT_Workfile  begsr
046700140916      *
046800140916     c                   exsr      NEWKSC
046900140916      *
047000140916     c                   clear                   WFTISE8C
047100140916     c                   eval      SE8CDAL  = dadata
047200140916     c                   eval      SE8CAL   =  adata
047300140916     c                   eval      SE8CNZD  = tasNZD
047400140916     c                   eval      SE8CKSC  = tasKSC
047500140916     c                   eval      SE8CKSCD = fldKSCd
047600140916     c                   eval      SE8CCLV  = fldCLV
047700140916     c                   eval      SE8CKSU  = fldKSU
047800140916     c                   eval      SE8CKSUD = fldKSUd
047900140916      *
048000140916      *   Agente / Agente Unificante
048100140916     c                   eval      SE8CAGE  = fldAGE
048200140916     c                   eval      SE8CAGU  = fldAGU
048300140916     c                   eval      SE8CAGUd = fldAGUd
048400140916      *    relativo ad Agente Unificante
048500140916     c                   eval      SE8CFIL  = fldFIL
048600140916     c                   eval      SE8CFILd = fldFILd
048700140916     c                   eval      SE8CARE  = fldARE
048800140916     c                   eval      SE8CAREd = fldAREd
048900140916     c                   eval      SE8CDIV  = fldDIV
049000140916     c                   eval      SE8CDIVd = fldDIVd
049100140916      *    Se ci sono state tariffe x DPD su cliente
049200140916     c                   eval      SE8CTDPD = fldTdpd
049300140916      *
049400140916     c                   exsr      TOT_Spediz
049500140916      *
049600140916     c                   write     WFTISE8C
049700140916      *
049800140916     c                   endsr
049900140916     ?****************************************************************************
050000140916      *?   Update      File di Work
050100140916     ?****************************************************************************
050200140916     c     UPD_Workfile  begsr
050300140916      *
050400140916     c                   exsr      TOT_Spediz
050500140916      *
050600140916     c                   update    WFTISE8C
050700140916      *
050800140916     c                   endsr
050900140916     ?****************************************************************************
051000140916      *?   Totalizza le spedizioni
051100140916     ?****************************************************************************
051200140916     c     ToT_Spediz    begsr
051300140916      *
051400140916      * aggiunge sempre una spedizione
051500140916     c                   eval      SE8CTOT1 = 1 + SE8CTOT1
051600140916      *
051700140918      * se la spedizione ha monocollo o non supera i 28 kg.di bollettato
051800140916      * aggiunge una spedizione
051900140918     c                   if        tasNCL = 1  and tasPKB <=28
052000140916     c                   eval      SE8CTOT2 = 1 + SE8CTOT2
052100140916     c                   end
052200140916      *
052300140916      * se la spedizione ha un solo collo e rimane entro i 28kg di bollettato
052400140916      *  e non ha COD -->(TASCBO="1")
052500140916      * aggiunge una spedizione
052600140917     c                   if        tasNCL = 1  and tasPKB <=28 and
052700140917     c                             tasCBO = '1'
052800140916     c                   eval      SE8CTOT3 = 1 + SE8CTOT3
052900140916     c                   end
053000140916      *
053100140916     c                   endsr
053200140916     ?****************************************************************************
