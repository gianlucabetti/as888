000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300140916     fWFTISE8CF uf a E           K DISK    usropn
000400140916-->  fTISE8CP   o    E             PRINTER OFLIND(*in66)
000500140916      *
000600140916     FAZORG01L  IF   E           K DISK    usropn
000700140916     FAZCMM01L  IF   E           K DISK    usropn
000800140916     FTNTAM01L  IF   E           K DISK
000900050510      **-----------------------------------------------------------------***
001000040820     d KPJBA         E DS
001100050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200110511     D CNCR42        E DS
001300140916     D dSTA01        E DS
001400040820     D*
001500140916-->  D DsPassaggio   e DS                  EXTNAME(TISE8BDS)
001600140916     D TITASds       e DS                  EXTNAME(TITAS00F)
001700040820     D*
001800040820     D WLBDAT          DS
001900040820     D  G02DAT                 1      8  0
002000040820     D  G02INV                 9     16  0
002100040820     D  G02ERR                17     17
002200040820     D  G02TGI                18     22  0
002300040820      *
002400040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002500040826     D  almeno_uno     s              1
002600140916     D*------------------
002700140916     D* DS REPERIMENTO DATI UTENTE
002800140916     D*-------------------
002900140916     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
003000140916     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
003100140916     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
003200140916     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
003300140916     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
003400140916      *
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
005100140916     D* DS DI PROCEDURA
005200140916     D*------------------
005300140916     D BS10DS        E DS                  EXTNAME(TIBS10DS)
005400140916     D*------------------
005500040820      *
005600050510     D  FLD_Distr      s                   like(orgfl3)
005700050510     D  FLD_Area       s                   like(orgcar)
005800050510     D  FLD_Fgs        s                   like(fttfgs)
005900140916     D  FLD_Padr       s                   like(apdpdr)
006000050905     D  FLD_Spediz     s              7  0
006100140916     D  FLD_Tot01      s              9  0
006200140916     D  FLD_Tot02      s              9  0
006300140916     D  FLD_Tot03      s              9  0
006400050510      *
006500050511     D  TOT_Dist       s                   like(M_FmeDist)
006600050511     D  TOT_Area       s                   like(M_FmeArea)
006700050511     D  TOT_POper      s                   like(M_FmePOper)
006800050510      *
006900050511     D*-------------
007000050511     D* Reperimento nome PGM
007100050511     D STATUS         SDS           333
007200050511     D  DSPGM            *PROC
007300050511     D*-------------
007400040820     D digits          C                   CONST('0123456789')
007500050511      * ?------------------------------------------------------------------ */
007600140916      * ? nel Periodo selezionato, si devono totalizzare le spedizioni export
007700140916      * ?      delle sole Linee EEX e solo in Porto Franco
007800140916      * ?  per CLIENTE MITTENTE e NAZIONE DESTINATARIO.
007900140916      *      ? Rilevando se il cliente ha avuto tariffe DPD
008000140916      * ? 1) Totalizzando il nr.di Spedizioni
008100140916      * ? 2) Totalizzando il nr.di Spedizioni con più colli o maggiori di 28 kg.
008200140916      * ? 3) Totalizzando il nr.di Spedizioni monocollo  minori uguali di 28 kg.
008300140916      * ?    e senza C.O.D.
008400140916      * ?------------------------------------------------------------------ */
008500140916      **  x ottimizzare l'SQL non fa Ordinamento
008600050511      * ?------------------------------------------------------------------ */
008700050510     C/EXEC SQL
008800050510     C+ Declare A1 Cursor for
008900140916     C+ select * from titas00f a where TASTBL = 'F1'        and
009000140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
009100140916     C+   taslna in
009200140916     C+   (SELECT orgfil FROM azorg00f WHERE orgde3 like '%EEX%' )
009300140916     C+ UNION all
009400140916     C+ select * from titas10f a where TASTBL = 'F1'        and
009500140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
009600140916     C+   taslna in
009700140916     C+   (SELECT orgfil FROM azorg00f WHERE orgde3 like '%EEX%' )
009800140916     C+ UNION all
009900140916     C+ select * from titas10f a where TASTBL = 'F1'        and
010000140916     C+   tasaas *10000+tasmgs between :DADATA  and :ADATA  and
010100140916     C+   taslna in
010200140916     C+   (SELECT orgfil FROM azorg00f WHERE orgde3 like '%EEX%' )
010300050510     C/END-EXEC
010400050510      *          apertura cursore
010500050510     C/EXEC SQL
010600050510     C+ OPEN A1
010700050510     C/END-EXEC
010800050510     c                   clear                   almeno_uno
010900050510      **
011000050510     C                   DOU       SqlCod <> 0
011100050510      **         lettura cursore
011200050510     C/EXEC SQL
011300140916     C+ FETCH NEXT FROM A1 INTO :TITASds
011400050510     C/END-EXEC
011500050510     C                   SELECT
011600050510     **
011700050510     C                   WHEN      SqlCod = 100
011800050510     c* a fine file
011900050510     c                   if        almeno_uno = 'S'
012000140916     c                   exsr      daFare_a_EoF
012100050510     c                   else
012200050510     c                   end
012300050510     c                   leave
012400050510     c*
012500050510     C                   WHEN      SqlCod < 0
012600050510     C                   seton                                        H1
012700050510     c                   goto      fine
012800050510     c*
012900050510     C                   OTHER
013000050510     **
013100050510     c* DETTAGLIO
013200050510     c* riga   dettaglio
013300140916     c                   exsr      ElabRig_det
013400050510     C                   ENDSL
013500050510
013600050510     C                   ENDDO
013700050510     C/EXEC SQL
013800050510     C+ Close A1
013900050510     C/END-EXEC
014000050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014100041007     c     fine          tag
014200020326     C                   SETON                                        LR
014300050511     ?****************************************************************************
014400140916      *?   Elabora Riga File di Work
014500050511     ?****************************************************************************
014600140916     c     ElabRig_det   begsr
014700050510      *
014800050511     c                   move      'S'           almeno_uno
014900050524      *
015000140916      *  Verifica esistenza del Cliente/Nazione Destinatario
015100140916      *    x andare in WRITE o in UPDATE
015200140916      *
015300140916     c     KEYworkF      chain     WFtise8CF
015400140916      *
015500140916     c                   If        %Found(WFtise8CF)
015600140916     c                   exsr      UPD_Workfile
015700140916      *
015800140916     c                   else
015900140916     c                   exsr      WRT_Workfile
016000140916      *
016100140916     c                   end
016200140916      *
016300140916     c                   endsr
016400140916     ?****************************************************************************
016500140916      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
016600140916     ?****************************************************************************
016700140916     c     daFare_a_EoF  begsr
016800140916      *
016900140916      *
017000140916     c                   endsr
017100050511     ?****************************************************************************
017200050511      *?   Stampa      File di Work Una riga x P.O.
017300050511     ?****************************************************************************
017400050511     c     PRT_rig_det   begsr
017500050511      *
017600050511     c                   if        *in66
017700050511      *                 * ------------------ *
017800050511     c                   write     testa
017900050511      *                 * ------------------ *
018000050511     c                   setoff                                       66
018100050511     c                   end
018200050511      *
018300050511     c                   eval      PDist   = TOT_Dist
018400050511     C                   eval      PArea   = TOT_Area
018500050511     C                   eval      PPOper  = TOT_POper
018600050511     c                   eval      PNumAut = TOT_NumAut
018700050511     c                   eval      PMedia  = TOT_MedGiorPO
018800050511      *
018900050511      *                 * ------------------ *
019000050511     c                   write     dettagli
019100050511      *                 * ------------------ *
019200050511      *
019300050511     c                   endsr
019400050511     ?****************************************************************************
019500050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
019600050511     ?****************************************************************************
019700050511     c     PRT_NO_Dati   begsr
019800050511      *
019900050511     c                   if        *in66
020000050511      *                 * ------------------ *
020100050511     c                   write     testa
020200050511      *                 * ------------------ *
020300050511     c                   setoff                                       66
020400050511     c                   end
020500050511      *
020600050511      *                 * ------------------ *
020700050511     c                   write     nodati
020800050511      *                 * ------------------ *
020900050511      *
021000050511     c                   endsr
021100050511     ?****************************************************************************
021200050511      *?   INIZIALIZZAZIONE
021300050511     ?****************************************************************************
021400040820     C     *inzsr        BEGSR
021500040820      **
021600140916     C     *LIKE         DEFINE    TASKSC        fldKSU
021700140916     C     *LIKE         DEFINE    acoRAG        fldKSUD
021800140916     C     *LIKE         DEFINE    acoRAG        fldKSCD
021900140916     C     *LIKE         DEFINE    ORGFIL        fldFIL
022000140916     C     *LIKE         DEFINE    ORGCAR        fldARE
022100140916     C     *LIKE         DEFINE    ORGFL3        fldDIV
022200140916     C     *LIKE         DEFINE    ORGFL3        fldDIC
022300140916     C     *LIKE         DEFINE    CLPCLV        fldCLV
022400140916     C     *LIKE         DEFINE    CLPAGE        fldAGU
022500140916     C     *LIKE         DEFINE    CMMDES        fldAGUd
022600140916     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
022700140916     C     *LIKE         DEFINE    TBLCOD        KTBCOD
022800140916     C     *LIKE         DEFINE    TBLKEY        KTBKEY
022900140916      **
023000140916     C     KEYworkF      KLIST
023100140916     C                   Kfld                    TASksc
023200140916     C                   Kfld                    TASnzd
023300050511      **
023400140916     C* CHIAVE LETTURA TABEL00F - COMPLETA
023500140916     C     keyTAB        KLIST
023600140916     C                   KFLD                    KTBKUT                         *CODICE UTENTE
023700140916     C                   KFLD                    KTBCOD                         *CODICE TABELLA
023800140916     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
023900140916     C*
024000040820     C     *ENTRY        PLIST
024100040820     C                   PARM                    KPJBA
024200050510     c                   movel     kpjbu         DsPassaggio
024300050511      **
024400050511     C                   Z-ADD     1             CODUT
024500050511     C                   CALL      'X§PARUT'
024600050511     C                   PARM                    UT§DSE
024700110511     C                   MOVEL     REC42         CNCR42
024800110511     C                   MOVEL     simRGS        RSUT             20
024900041103      *
025000040820      *  imposta UDATE di elaborazione
025100040820     C                   move      *date         G02DAT
025200040820     C                   MOVE      *ZEROS        G02INV
025300040820     C                   MOVE      *BLANKS       G02ERR
025400040820     C                   CALL      'XSRDA8'
025500040820     C                   PARM                    WLBDAT
025600040820     C                   Z-ADD     G02INV        data_oggi
025700050511     C                   move      G02dat        day
025800040820     C                   TIME                    W0140            14 0
025900050511     C                   MOVEl     W0140         ore
026000140916     C                   MOVE      W0140         G08DAT            8 0          *DATA (8) IN G/M/AA
026100140916     C                   Z-ADD     *ZEROS        G08INV
026200140916     C                   MOVEL     '0'           G08ERR
026300140916     C                   CALL      'XSRDA8'
026400140916     C                   PARM                    WLBDA8
026500140916     C                   Z-ADD     G08INV        DATAcorrente      8 0          *DATA (8) IN AA/M/G
026600140916      **
026700050510     c                   move      PASdti        dadata            8 0
026800050510     c                   move      PASdtf        adata             8 0
026900040823      **
027000050511     c                   seton                                        66
027100040825     c                   clear                   prima_volta       1
027200040826     c                   clear                   almeno_uno
027300050511     C                   MOVEL     DSPGM         NOMPGM
027400050511      **
027500050511     C                   move      *zeros        G02DAT
027600050511     C                   MOVE      dadata        G02INV
027700050511     C                   MOVE      '3'           G02ERR
027800050511     C                   CALL      'XSRDA8'
027900050511     C                   PARM                    WLBDAT
028000050511     C                   Z-ADD     G02Dat        PDadata
028100050511      **
028200050511     C                   move      *zeros        G02DAT
028300050511     C                   MOVE      adata         G02INV
028400050511     C                   MOVE      '3'           G02ERR
028500050511     C                   CALL      'XSRDA8'
028600050511     C                   PARM                    WLBDAT
028700050511     C                   Z-ADD     G02dat        PAdata
028800050510     c*
028900040820     C                   ENDSR
029000140916     ?**************************************************************************
029100140916     C* CARTAB - CARICA I DATI IN TABELLE
029200140916     ?**************************************************************************
029300140916     C     CARTAB        BEGSR
029400140916     C*--------------------
029500140916     C* ORGANIGRAMMA
029600140916     C*--------------------
029700140916     C                   Z-ADD     *ZEROS        F                 4 0
029800140916     c                   open      azORG01l
029900140916     C     *loval        SETLL     azORG01L
030000140916     C                   READ      azORG01L
030100140916      *
030200140916DO  1C                   DOW       Not %EoF(azORG01L)
030300140916      *
030400140916IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
030500140916IF  3C     orgFAG        andNE     'V'                                          *FILIALE/AGENZIA
030600140916     C                   ADD       1             F
030700140916      *-  Filiale
030800140916     C                   Z-ADD     orgFIL        FIL(F)
030900140916     C                   MOVEL     orgDES        FILD(F)
031000140916      *-  Area
031100140916     C                   Z-ADD     1             KTBKUT
031200140916     C                   MOVEL     '05'          KTBCOD
031300140916     C                   MOVEL     *BLANKS       KTBKEY
031400140916     C                   MOVEL     orgCAR        KTBKEY
031500140916     C     keyTAB        CHAIN     TABEL00F
031600140916IF  5C                   IF        %Found(TABEL00F)
031700140916     C                   Z-ADD     orgCAR        ARE(F)
031800140916     C                   MOVEL     TBLUNI        ARED(F)
031900140916E   5C                   ENDIF
032000140916      *-  Divisione
032100140916     C                   Z-ADD     1             KTBKUT
032200140916     C                   MOVEL     '17'          KTBCOD
032300140916     C                   MOVEL     *BLANKS       KTBKEY
032400140916     C                   MOVEL     orgFL3        KTBKEY
032500140916     C     keyTAB        CHAIN     TABEL00F
032600140916IF  5C                   IF        %Found(TABEL00F)
032700140916     C                   MOVEL     orgFL3        DIV(F)
032800140916     C                   MOVEL     TBLUNI        DIVD(F)
032900140916E   5C                   ENDIF
033000140916      *-
033100140916E   3C                   ENDIF
033200140916     C                   READ      azORG01L                               99
033300140916E   1C                   ENDDO
033400140916     c                   close     azORG01l
033500140916     C*--------------------
033600140916     C* COMMERCIALI e UNIficanti
033700140916     C*--------------------
033800140916     C                   Z-ADD     *ZEROS        C                 4 0
033900140916     c                   open      azCMM01l
034000140916     C     *loval        SETLL     azCMM01l
034100140916     C                   READ      azCMM01l
034200140916DO  1C                   DOW       not %Eof(azCMM01l)
034300140916IF  2C                   IF        cmmATB = *blank
034400140916     C                   ADD       1             C
034500140916     C                   z-add     cmmCOD        AGE(C)
034600140916     C                   z-add     cmmUNI        AGU(C)
034700140916     C                   MOVEL     cmmDES        DAGE(C)
034800140916E   2C                   ENDIF
034900140916     C                   READ      azCMM01l
035000140916E   1C                   ENDDO
035100140916     c                   close     azCMM01l
035200140916     C*
035300140916     C                   ENDSR
035400140916      *------------------------------------------------------------------------*
035500140916      * NEWKSC - OPERAZIONI PER NUOVO CLIENTE
035600140916      *------------------------------------------------------------------------*
035700140916     C     NEWKSC        BEGSR
035800140916      *
035900140916      *  pulizia campi di work
036000140916     C                   clear                   fldKSCd
036100140916     C                   clear                   fldKSU
036200140916     C                   clear                   fldKSUd
036300140916     C                   clear                   fldCLV
036400140916     C                   clear                   fldAGE                         *CODICE AGENTE
036500140916     C                   clear                   fldAGU                         *CODICE AGENTE
036600140916     C                   clear                   fldAGUd
036700140916     C                   clear                   fldFILd
036800140916     C                   clear                   fldAREd
036900140916     C                   clear                   fldDIVd
037000140916     C                   clear                   fldFIL
037100140916     C                   clear                   fldARE
037200140916     C                   clear                   fldDIV
037300140916     C                   clear                   fldTdpd
037400140916      *
037500140916      *  Codifica il cliente
037600140916     C                   CLEAR                   BS69DS
037700140916     C                   CLEAR                   CLPDS
037800140916     C                   CLEAR                   ACODS
037900140916     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
038000140916     C                   MOVEL     tasKSC        I69ACO                         *CLIENTE X CNACO
038100140916     C                   CALL      'TIBS69R'
038200140916     C                   PARM                    BS69DS
038300140916     C                   PARM                    ACODS
038400140916     C                   PARM                    INDDS
038500140916     C                   PARM                    CLPDS
038600140916     C                   PARM                    CLSDS
038700140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
038800140916     C                   MOVEL     *ALL'*'       fldKSCD
038900140916X   1C                   ELSE                                                   *NO ERRORE
039000140916     C                   Z-ADD     acoRAG        fldKSCD
039100140916E   1C                   ENDIF
039200140916     C* IMPOSTA UNIFICANTE
039300140916     c                   Z-ADD     tasKSC        fldKSU
039400140916      *
039500140916     C                   CLEAR                   BS10DS
039600140916     C                   Z-ADD     DATAcorrente  D10DRF                         *DATA RIFERIMENTO
039700140916     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
039800140916     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
039900140916     C                   MOVE      tasKSC        D10COD
040000140916     C                   CALL      'TIBS10R'
040100140916     C                   PARM                    BS10DS
040200140916      *
040300140916IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
040400140916     C     D10COP        ANDNE     *ZEROS
040500140916     C                   Z-ADD     D10COP        fldKSU                         *UNIFICANTE
040600140916E   2C                   ENDIF
040700140916      *
040800140916      * REPERISCE IL COMMERCIALE,
040900140916      *  SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
041000140916     C                   CLEAR                   BS69DS
041100140916     C                   CLEAR                   CLPDS
041200140916     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
041300140916     C                   MOVEL     fldKSU        I69KCP                         *CLIENTE X CNCLP
041400140916     C                   MOVEL     fldKSU        I69ACO                         *CLIENTE X CNACO
041500140916     C                   CALL      'TIBS69R'
041600140916     C                   PARM                    BS69DS
041700140916     C                   PARM                    ACODS
041800140916     C                   PARM                    INDDS
041900140916     C                   PARM                    CLPDS
042000140916     C                   PARM                    CLSDS
042100140916      *
042200140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
042300140916     C                   MOVEL     *ALL'*'       fldKSUD
042400140916X   1C                   ELSE                                                   *NO ERRORE
042500140916     C                   Z-ADD     acoRAG        fldKSUD
042600140916      *
042700140916      * REPERISCE IL CODICE QUALITA'
042800140916     C                   MOVEL     'C'           fldCLV
042900140916IF  1C     CLPCLV        ifne      *BLANKS
043000140916     C                   MOVEL     CLPCLV        fldCLV
043100140916     c                   end
043200140916E   1C                   ENDIF
043300140916      *
043400140916IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
043500140916     C     CLPAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
043600140916     C                   MOVEL     fldKSU        NUM3              3 0          *CODICE FILIALE +
043700140916     C                   Z-ADD     *ZEROS        fldAGE                         *AGENTE A ZERO  =
043800140916     C                   MOVEL     NUM3          fldAGE                         *FIL+0000
043900140916X   1C                   ELSE                                                   *NO ERRORE
044000140916     C                   Z-ADD     CLPAGE        fldAGE                         *CODICE AGENTE
044100140916E   1C                   ENDIF
044200140916      *
044300140916      * REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
044400140916     C                   Z-ADD     1             C
044500140916     C                   SETOFF                                           99
044600140916     C     fldAGE        LOOKUP    AGE(C)                                 99
044700140916      *
044800140916IF  1C     *IN99         IFEQ      *OFF
044900140916     C                   Z-ADD     fldAGE        fldAGU                         RAGGRUPP = COD AGENT
045000140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGRUP
045100140916X   1C                   ELSE
045200140916      *
045300140916IF  2C     AGU(C)        IFEQ      *ZEROS
045400140916     C                   Z-ADD     fldAGE        fldAGU
045500140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGRUP
045600140916X   2C                   ELSE
045700140916      *
045800140916      * DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
045900140916     C                   Z-ADD     1             C
046000140916     C                   Z-ADD     AGU(C)        fldAGU
046100140916     C                   SETOFF                                           99
046200140916     C     fldAGU        LOOKUP    AGE(C)                                 99
046300140916IF  3C     *IN99         IFEQ      *ON
046400140916     C                   MOVEL     DAGE(C)       fldAGUd                        DESCRIZIONE RAGGR.TO
046500140916X   3C                   ELSE
046600140916     C                   MOVEL     *ALL'*'       fldAGUd                        DESCRIZIONE RAGGR.TO
046700140916E   3C                   ENDIF
046800140916E   2C                   ENDIF
046900140916E   1C                   ENDIF
047000140916      *
047100140916      * REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
047200140916     C                   Z-ADD     1             F
047300140916     C                   MOVEL     fldAGU        fldFIL                         *FIL+XXXX
047400140916     C                   SETOFF                                           99
047500140916     C     fldFIL        LOOKUP    FIL(F)                                 99
047600140916IF  1C     *IN99         IFEQ      *ON
047700140916     C                   MOVEL     FILD(F)       fldFILd
047800140916     C                   Z-ADD     ARE(F)        fldARE
047900140916     C                   MOVEL     ARED(F)       fldAREd
048000140916     C                   MOVEL     DIV(F)        fldDIV
048100140916     C                   MOVEL     DIVD(F)       fldDIVd
048200140916E   1C                   ENDIF
048300140916      *
048400140916      *  Controlla se il cliente ha o ha avuto delle tariffe DPD
048500140916     c     tasKSC        setll     TNTAM01L
048600140916     c     tasKSC        reade     TNTAM01L
048700140916     c                   DoW       not %EoF(TNTAM01L)
048800140916      *
048900140916      * solo tar.Italia x DPD
049000140916     c                   if        tamFIE = 'I'
049100140916     c                   eval        dSTA01 = tamFLO
049200140916     c                   if            §taDPD = 'S'
049300140916     C                   eval            fldTdpd = 'S'
049400140916     c                   leave
049500140916     c                   end
049600140916     c                   end
049700140916      *
049800140916     c     tasKSC        reade     TNTAM01L
049900140916     c                   endDO
050000140916      *
050100140916     C                   ENDSR
050200140916     ?****************************************************************************
050300140916      *?   Scrive      File di Work
050400140916     ?****************************************************************************
050500140916     c     WRT_Workfile  begsr
050600140916      *
050700140916     c                   exsr      NEWKSC
050800140916      *
050900140916     c                   clear                   WFTISE8C
051000140916     c                   eval      SE8CDAL  = dadata
051100140916     c                   eval      SE8CAL   =  adata
051200140916     c                   eval      SE8CNZD  = tasNZD
051300140916     c                   eval      SE8CKSC  = tasKSC
051400140916     c                   eval      SE8CKSCD = fldKSCd
051500140916     c                   eval      SE8CCLV  = fldCLV
051600140916     c                   eval      SE8CKSU  = fldKSU
051700140916     c                   eval      SE8CKSUD = fldKSUd
051800140916      *
051900140916      *   Agente / Agente Unificante
052000140916     c                   eval      SE8CAGE  = fldAGE
052100140916     c                   eval      SE8CAGU  = fldAGU
052200140916     c                   eval      SE8CAGUd = fldAGUd
052300140916      *    relativo ad Agente Unificante
052400140916     c                   eval      SE8CFIL  = fldFIL
052500140916     c                   eval      SE8CFILd = fldFILd
052600140916     c                   eval      SE8CARE  = fldARE
052700140916     c                   eval      SE8CAREd = fldAREd
052800140916     c                   eval      SE8CDIV  = fldDIV
052900140916     c                   eval      SE8CDIVd = fldDIVd
053000140916      *    Se ci sono state tariffe x DPD su cliente
053100140916     c                   eval      SE8CTDPD = fldTdpd
053200140916      *
053300140916     c                   exsr      TOT_Spediz
053400140916      *
053500140916     c                   write     WFTISE8C
053600140916      *
053700140916     c                   endsr
053800140916     ?****************************************************************************
053900140916      *?   Update      File di Work
054000140916     ?****************************************************************************
054100140916     c     UPD_Workfile  begsr
054200140916      *
054300140916     c                   exsr      TOT_Spediz
054400140916      *
054500140916     c                   update    WFTISE8C
054600140916      *
054700140916     c                   endsr
054800140916     ?****************************************************************************
054900140916      *?   Totalizza le spedizioni
055000140916     ?****************************************************************************
055100140916     c     ToT_Spediz    begsr
055200140916      *
055300140916      * aggiunge sempre una spedizione
055400140916     c                   eval      SE8CTOT1 = 1 + SE8CTOT1
055500140916      *
055600140916      * se la spedizione ha più colli o supera i 28 kg.di bollettato
055700140916      * aggiunge una spedizione
055800140916     c                   if        tasNCL > 1  or tasPKB > 28
055900140916     c                   eval      SE8CTOT2 = 1 + SE8CTOT2
056000140916     c                   end
056100140916      *
056200140916      * se la spedizione ha un solo collo e rimane entro i 28kg di bollettato
056300140916      *  e non ha COD -->(TASCBO="1")
056400140916      * aggiunge una spedizione
056500140916     c                   if        tasNCL = 1  or tasPKB <=28
056600140916     c                   if        tasCBO = '1'
056700140916     c                   eval      SE8CTOT3 = 1 + SE8CTOT3
056800140916     c                   end
056900140916     c                   end
057000140916      *
057100140916     c                   endsr
057200140916     ?****************************************************************************
