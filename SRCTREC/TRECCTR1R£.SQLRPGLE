000100000000      *------------------------------------------------------------------------*
000200110505      *  controlla il file ECEVD00F
000300110505      *  dell'eventuale libreria (non obb.) passata come parametro di INPUT
000400110505      *  gli altri archivi sono quelli nella lista di librerie
000500000000      *------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FECEVT01L  IF   E           K DISK
001200131128     FECEVD06l  IF   E           K DISK    prefix(£)
001300100422     FECEVM01l  iF   E           K DISK
001400000000     FTABEL00F  IF   E           K DISK
001500941128     FTNTAM01L  IF   E           K DISK
001600140522     Fazcpc01L  IF   E           K DISK
001700110418     Fprtf198   o    f  198        printer oflind(*inof)
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100140522     D MSG             S             78    DIM(48) CTDATA PERRCD(1)
002200110412     D $msg            S             78
002300110412     D $err            S              1
002400000000     D*-------------------
002500020923     D* DS REPERIMENTO DATI UTENTE
002600020923     D*-------------------
002700020923     D TIBS34DS      E DS
002800020923     D DDATIUTE      E DS
002900020923     D AZUTEDS       E DS                  extname(AZUTE00F)
003000110406     D ecevdds       E DS                  extname(ecevd00F)
003100000000     D*---------------------
003200000000     D* DS ESTERNA DI PROCEDURA
003300000000     D*---------------------
003400000000     D DS60          E DS                  EXTNAME(TREC60DS)
003500970627     D*---------------------
003600970627     D* DS ESTERNA DI PROCEDURA
003700970627     D*---------------------
003800970627     D DSSI95        E DS                  EXTNAME(TISI95DS)
003900971020     D*------------------
004000971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
004100971020     D*------------------
004200971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
004300000000     D*-------------------
004400000000     D* ARCHITETTURA
004500000000     D*-------------------
004600000000     D KPJBA         E DS
004700000000     D*------------------
004800000000     D* CALL XSRDA8 - CONTROLLO DATE
004900000000     D*------------------
005000000000     D WLBDAT          DS                  INZ
005100000000     D  G02DAT                 1      8  0
005200000000     D  G02INV                 9     16  0
005300000000     D  G02ERR                17     17
005400000000     D  G02TGI                18     22  0
005500000000     D*------------------
005600000000     D* TABELLE DI DECODIFICA
005700000000     D*------------------
005800000000     D DS2Q          E DS
005900000000     D DS2R          E DS
006000100422     D DS2O          E DS
006100140522     D*-------------------
006200140522     D* DS ESTERNA TABELLA 15 (CODICI nazioni)
006300140522     D*-------------------
006400140522     D DS15          E DS
006500000000     D*------------------
006600950718     D* PARAMETRI CHIAMATA 'TNTA36R'
006700000000     D*------------------
006800000000     D PARAM4          DS                  INZ
006900000000     D  PARCOD                 6     12  0
007000000000     D  PARCTR                13     15
007100000000     D  PARFLG                16     16
007200000000     D  PARID                 17     17  0
007300000000     D  PARCCC                18     21  0
007400000000     D  PARDCV                22     36
007500000727     D*------------------
007600000727     D* DS REPERIMENTO DATI UTENTE
007700000727     D*-------------------
007800000727     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007900000727     D ACODS         E DS                  EXTNAME(CNACO00F)
008000000727     D INDDS         E DS                  EXTNAME(CNIND00F)
008100000727     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008200000727     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008300000000     C*------------------------------------------------------------------------*
008400000000     C* MAIN LINE
008500000000     C*------------------------------------------------------------------------*
008600110404     C*
008700110405     C/EXEC SQL
008800131128     C+ DECLARE E1 CURSOR FOR SELECT * FROM ecevd06l ORDER BY EVDVOC,
008900131128     C+ EVDPRG, EVDTTV, EVDFT1, EVDFT2, EVDCLI, EVDCTR, EVDCTS, EVDnaz,
009000131128     C+ evdcad, EVDDDT, EVDSGI
009100110405     C/END-EXEC
009200110405     c*
009300110405     C/EXEC SQL
009400110405     C+ OPEN e1
009500110405     C/END-EXEC
009600110405
009700110405     C                   DO        *HIVAL
009800110405
009900110405     C/EXEC SQL
010000110405     C+ FETCH NEXT FROM e1 INTO :ecevdds
010100110405     C/END-EXEC
010200110405
010300110405     C                   IF        SQLCod = 100 or sqlcod < 0
010400110405     C                   LEAVE
010500110405     C                   ENDIF
010600110419     c* se la tariffa è scaduta non la controllo
010700110419     c                   if        evddst < datcor
010800110419     C                   iter
010900110419     C                   ENDIF
011000110419     c* se la tariffa è annullata non la controllo
011100110419     c                   if        evdatb <> ' '
011200110419     C                   iter
011300110419     C                   ENDIF
011400110405     c                   exsr      decodi
011500110405     C                   ENDDO
011600110405
011700110405     C/EXEC SQL
011800110405     C+ CLOSE e1
011900110405     C/END-EXEC
012000000000     C*
012100000000     C                   SETON                                        LR
012200000000     C*------------------------------------------------------------------------*
012300000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
012400000000     C*------------------------------------------------------------------------*
012500000000     C     DECODI        BEGSR
012600000000     C* CODICE VOCE
012700110405     C                   Z-ADD     evdVOC        KVTVOC
012800110405     C                   Z-ADD     evdPRG        KVTPRG
012900110406     C     KEYEVT        CHAIN     ECEVT01L
013000110406     C                   IF        not %found(ecevt01l)
013100000000     C                   MOVEL     '1'           $ERR
013200000000     C                   MOVEL     MSG(7)        $MSG
013300110405     C                   EXSR      fine
013400110412     c                   leavesr
013500110419     c                   else
013600110419     c* se la voce è scaduta non la controllo
013700110419     c                   if        evtdsv < datcor
013800110419     c                   leavesr
013900110405     C                   End
014000110419     c* se la voce è annulata non la controllo
014100110419     c                   if        evtatb <> ' '
014200110419     c                   leavesr
014300110419     C                   End
014400110419     C                   End
014500100422     C* AGGANCIO LA TABELLA 2O in caso di maggiorazione
014600100422     c* se x voce isolamente le tariffe devono essere tutte positivi o
014700100422     c* tutte negative
014800100422     c                   clear                   flgsegtar         1
014900100422     c                   if        evttvo = 'M'
015000100422     C                   MOVEL     1             KTBKUT
015100100422     C                   MOVEL     '2O'          KTBCOD
015200100422     C                   MOVEL     'MI'          KTBKEY
015300110412     C     KEYTAB        CHAIN     TABEL00F
015400110412     C                   IF        not %found(tabel00f) or
015500110412     C                             TBLFLG <> *BLANKS
015600100422     C                   ELSE
015700100422     C                   MOVEL     TBLUNI        ds2O
015800110405     C                   if        §2ovocda <= evdvoc and
015900110405     C                             §2ovoca >= evdvoc
016000100422     c                   eval      flgsegtar='1'
016100100422     c                   end
016200100422     c                   end
016300110405     c                   end
016400110405     C* AGGANCIO LA TABELLA 2R PER IL TIPO TARIFFA
016500110405     C                   MOVEL     1             KTBKUT
016600110405     C                   MOVEL     '2R'          KTBCOD
016700110405     C                   MOVE      *BLANKS       KTBKEY
016800110405     C                   MOVEL     EVTVRV        KTBKEY
016900110412     C     KEYTAB        CHAIN     TABEL00F
017000110412     C                   IF        not %found(tabel00f) or
017100110412     C                             TBLFLG <> *BLANKS
017200110405     C                   CLEAR                   DS2R
017300110405     C                   ELSE
017400110405     C                   MOVEL     TBLUNI        DS2R
017500110405     c                   end
017600000000     C* NELLA DS2R C'E' UN FLAG PER CONSENTIRE UN SOLO, O PIU', SCAGLIONE
017700000000     C* CONTROLLO DA FARE SOLO IN INSERIMENTO
017800110412     C     §2RPIU        IFNE      'S'
017900110405     C                   MOVE      evdVOC        KVDVOC
018000110405     C                   MOVE      evdPRG        KVDPRG
018100110405     C                   MOVE      evdTTV        KVDTTV
018200131128     C     KE3EVD        SETLL     ECEVD06l
018300131128     C     KE3EVD        READE     ECEVD06l                               99
018400110412     C     *IN99         DOWEQ     *OFF
018500110412     C     £EVDATB       IFEQ      *BLANKS
018600110412     C     £EVDSGI       andne     evdSGI
018700000000     C                   MOVEL     '1'           $ERR
018800000000     C                   MOVEL     MSG(24)       $MSG
018900110406     C                   EXSR      fine
019000110412     c                   leavesr
019100110412     C                   ENDIF
019200131128     C  N99KE3EVD        READE     ECEVD06L                               99
019300110412     C                   ENDDO
019400110412     c                   ENDIF
019500000000     C* TIPO TARIFFA
019600000000     C                   MOVEL     '2S'          §COD
019700000000     C                   MOVEL     *BLANKS       §KEY
019800110406     C                   MOVEL     evdTTV        §KEY
019900000000     C                   EXSR      CHKTAB
020000000000     C*?    V O C E    D I    C L I E N T E    ?
020100110412     C     evdTTV        IFEQ      'K'
020200000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X CLIENTE NON POSSO
020300000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'K'
020400000000     C     EVTRKI        IFEQ      *BLANKS
020500000000     C     EVTRKE        ANDEQ     *BLANKS
020600000000     C                   MOVEL     '1'           $ERR
020700000000     C                   MOVEL     MSG(40)       $MSG
020800110406     C                   EXSR      fine
020900110412     c                   leavesr
021000000000     C                   ENDIF
021100000727     C* CONTROLLA VALIDITA' CODICE
021200000727     C                   CLEAR                   BS69DS
021300000727     C                   CLEAR                   ACODS
021400000727     C                   MOVEL     KNSIF         I69SIF
021500110406     C                   MOVEL     evdCLI        I69KAC
021600000727     C                   CALL      'TIBS69R'
021700000727     C                   PARM                    BS69DS
021800000727     C                   PARM                    ACODS
021900000727     C                   PARM                    INDDS
022000000727     C                   PARM                    CLPDS
022100000727     C                   PARM                    CLSDS
022200110406     c                   if        o69err = '1'
022300110406     C                   MOVEL     '1'           $ERR
022400110406     C                   MOVEL     MSG(30)       $MSG
022500110406     C                   EXSR      fine
022600110412     c                   leavesr
022700110406     c                   end
022800000000     C*--------------------
022900000000     C* TARIFFA CLIENTE
023000000000     C*--------------------
023100110406     C                   MOVEL     evdCTR        $CTR              3
023200000000     C                   EXSR      CHKCTR
023300940722     C* ERRORE (CARATTERI ALFABETICI)
023400110412     C     $ERR          IFEQ      '1'
023500110412     C                   MOVEL     MSG(12)       $MSG
023600110406     C                   EXSR      fine
023700110412     c                   leavesr
023800110412     C                   ENDIF
023900110418     C                   ENDIF
024000110412     C*?    V O C E    D I    C. A. P.    ?
024100110412     C     evdTTV        IFEQ      'P'
024200000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X C.A.P.  NON POSSO
024300000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'P'
024400000000     C     EVTRPI        IFEQ      *BLANKS
024500000000     C     EVTRPE        ANDEQ     *BLANKS
024600000000     C                   MOVEL     '1'           $ERR
024700000000     C                   MOVEL     MSG(40)       $MSG
024800110406     C                   EXSR      fine
024900110412     c                   leavesr
025000110412     C                   ENDIF
025100140522     C*--------------------
025200140522     C* CODICE C.A.P.
025300140522     C*--------------------
025400140522IF   C     $ERR          IFEQ      '0'
025500140522      **   essendo ITALIA definita sulla tab 15 come Blk
025600140522      **   deve sempre testare
025700140522     C                   MOVEL     1             KTBKUT
025800140522     C                   MOVEL     '15'          KTBCOD
025900140522     C                   MOVEl     evdnaz        ktbkey
026000140522     C                   CLEAR                   DS15
026100140522     C     KEYTAB        CHAIN     TABEL00F                           99
026200140522     C                   IF        %found(tabel00f) and
026300140522     C                             tblflg = *BLANKS
026400140522     C                   MOVEL     TBLUNI        DS15
026500140522X    C                   ElsE
026600140522     C                   MOVEL     '1'           $ERR
026700140522     C                   MOVEL     MSG(48)       $MSG
026800140522     C                   EXSR      fine
026900140523     c                   leavesr
027000140522E    C                   ENDIF
027100140522E    C                   ENDIF
027200140522      * x Italia
027300140522IF   C     $ERR          IFEQ      '0'
027400140522IF   C                   IF        evdnaz = *blank and EVTRPI ='PP'
027500140522     c                               or
027600140522     C                             evdnaz <>*blank and EVTRPE ='PP'
027700140522     c*
027800140522     c                   if         %len(%trim(%subst(evdCAD:§15pcf)))
027900140522     c                                  > §15ecf
028000140522     C                   MOVEL     '1'           $ERR
028100140522     C                   MOVEL     MSG(47)       $MSG
028200140522     C                   EXSR      fine
028300140523     c                   leavesr
028400140522     c                   endif
028500140522E    C                   endIF
028600140522E    C                   endIF
028700140522      *
028800140522     C     $ERR          IfeQ      '0'
028900140522     C                   MOVEL(p)  evdNAZ        WNAR
029000140522     C                   MOVEL(P)  evdCAd        WCAP
029100140522     C                   Z-ADD     *ZEROS        WDAT
029200140522     C                   EXSR      CHKCAP
029300140522IF   C     $ERR          IFEQ      '1'
029400140522     C                   MOVEL     MSG(10)       $MSG
029500140522     C                   EXSR      fine
029600140523     c                   leavesr
029700140522E    C                   ENDIF
029800140522E    C                   ENDIF
029900110412     C                   ENDIF
030000000000     C*?    V O C E    D I    T A S S A Z I O N E    ?
030100110412     C     evdTTV        IFEQ      'T'
030200000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X TASSAZIONE NON POSSO
030300000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'T'
030400000000     C     EVTRTI        IFEQ      *BLANKS
030500000000     C     EVTRTE        ANDEQ     *BLANKS
030600000000     C                   MOVEL     '1'           $ERR
030700000000     C                   MOVEL     MSG(40)       $MSG
030800110406     C                   EXSR      fine
030900110412     c                   leavesr
031000000000     C                   ENDIF
031100000000     C*--------------------
031200000000     C* CODICE TASSAZIONE
031300000000     C*--------------------
031400000000     C                   MOVE      'CT'          §COD
031500000000     C                   MOVEL     *BLANKS       §KEY
031600110406     C                   MOVEL     evdCTS        §KEY
031700000000     C                   EXSR      CHKTAB
031800110412     C     $ERR          IFEQ      '1'
031900000000     C                   MOVEL     MSG(16)       $MSG
032000110406     C                   EXSR      fine
032100110412     c                   leavesr
032200110412     C                   ENDIF
032300110412     C                   ENDIF
032400110412     C*?    V O C E    D I    F I L I A L E    ?
032500110412     C     evdTTV        IFEQ      'F'
032600000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X FILIALE NON POSSO
032700000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'F'
032800000000     C     EVTRFI        IFEQ      *BLANKS
032900000000     C     EVTRFE        ANDEQ     *BLANKS
033000000000     C                   MOVEL     '1'           $ERR
033100000000     C                   MOVEL     MSG(40)       $MSG
033200110412     C                   EXSR      fine
033300110412     c                   leavesr
033400000000     C                   ENDIF
033500000000     C*--------------------
033600000000     C* CODICE FILIALE/TERMINAL
033700000000     C*--------------------
033800000000     C* AGGANCIO LA TABELLA 2Q CON IL RIFERIMENTO TARIFFA (ITALIA/ESTERO)
033900000000     C* E' SUFFICIENTE FARLO SOLO PER L'ITALIA, IN QUANTO I FLAG PER L'ITALIA E
034000000000     C* PER L'ESTERO DEVONO ESSERE UGUALI
034100020923     C                   MOVEL     1             KTBKUT
034200000000     C                   MOVEL     '2Q'          KTBCOD
034300000000     C                   MOVE      *BLANKS       KTBKEY
034400940728IF  4C     EVTRFI        IFNE      *BLANKS
034500000000     C                   MOVEL     EVTRFI        KTBKEY
034600940728X   4C                   ELSE
034700000000     C                   MOVEL     EVTRFE        KTBKEY
034800940728E   4C                   ENDIF
034900000000     C     KEYTAB        CHAIN     TABEL00F                           99
035000940728IF  4C     *IN99         IFEQ      '1'
035100000000     C     TBLFLG        ORNE      *BLANKS
035200000000     C                   CLEAR                   DS2Q
035300940728X   4C                   ELSE
035400000000     C                   MOVEL     TBLUNI        DS2Q
035500940728E   4C                   ENDIF
035600000000     C*
035700110412     C     evdFT1        IFGE      *ZERO
035800110412     C     evdft1        CHAIN     AZORG01L
035900110418     C                   IF        not %found(azorg01l)
036000000000     C                   MOVEL     '1'           $ERR
036100000000     C                   MOVEL     MSG(14)       $MSG
036200110412     C                   EXSR      fine
036300110412     c                   leavesr
036400110412     C                   ENDIF
036500110412     C                   ELSE
036600000000     C                   MOVEL     '1'           $ERR
036700000000     C                   MOVEL     MSG(14)       $MSG
036800110412     C                   EXSR      fine
036900110412     c                   leavesr
037000110412     C                   ENDIF
037100000000     C* 2° FILIALE
037200110412     C     evdFT2        IFGT      *ZERO
037300000000     C     §2QFAR        ORNE      *BLANKS
037400110405     C     evdft2        CHAIN     AZORG01L                           65
037500110412     C     *IN65         IFEQ      *ON
037600000000     C                   MOVEL     '1'           $ERR
037700000000     C                   MOVEL     MSG(14)       $MSG
037800110412     C                   EXSR      fine
037900110412     c                   leavesr
038000110412     C                   ELSE
038100000000     C* SE NELLA 2Q NON C'E' IL FLAG §2QFAR, NON DEVE ESSERCI IL COD.FILIALE
038200110412     C     §2QFAR        IFEQ      *BLANKS
038300110406     C     evdFT2        ANDNE     *ZERO
038400000000     C                   MOVEL     '1'           $ERR
038500000000     C                   MOVEL     MSG(38)       $MSG
038600110412     C                   EXSR      fine
038700110412     c                   leavesr
038800110412     C                   ENDIF
038900110412     C                   ENDIF
039000110412     C                   ENDIF
039100110412     C                   ENDIF
039200000000     C*?    CAMPI  COMUNI  A  TUTTI  I  TIPI  DI  TARIFFA     ?
039300000000     C*--------------------
039400000000     C* DATA DECORRENZA (SOLO SE INSERIMENTO)
039500000000     C*--------------------
039600110406     C                   Z-ADD     evdDDT        $DAT              8 0
039700000000     C                   EXSR      CHKDAT
039800110412     C     $ERR          IFEQ      '1'
039900110412     C                   EXSR      fine
040000110412     c                   leavesr
040100110412     C                   ENDIF
040200000000     C*--------------------
040300000000     C* DATA SCADENZA
040400000000     C*--------------------
040500110406     C                   Z-ADD     evdDST        $DAT
040600000000     C                   EXSR      CHKDAT
040700110412     C     $ERR          IFEQ      '1'
040800110412     C                   EXSR      fine
040900110412     c                   leavesr
041000110412     C                   ENDIF
041100000000     C* MAGGIORE DELLA DECORRENZA
041200110412     C     evddst        IFLT      evdddt
041300000000     C                   MOVEL     '1'           $ERR
041400000000     C                   MOVEL     MSG(23)       $MSG
041500110412     C                   EXSR      fine
041600110412     c                   leavesr
041700100330     c                   else
041800110406     c* se voce da isolare non può avere una scadenza
041900110406     c                   if        evtvrv = '26' and evddst <> 20391231
042000100330     C                   MOVEL     '1'           $ERR
042100100330     C                   MOVEL     MSG(31)       $MSG
042200110412     C                   EXSR      fine
042300110412     c                   leavesr
042400000000E   2C                   ENDIF
042500100330E   2C                   ENDIF
042600000000     C*--------------------
042700000000     C* VALIDITA' VOCE PER NON AVERE PERIODI ACCAVALLATI IN ARCHIVIO
042800000000     C*--------------------
042900110412     C     *LIKE         DEFINE    evdsgi        sgiv1c
043000110412     C     *LIKE         DEFINE    evdddt        ddtv1c
043100110412     C     *LIKE         DEFINE    evddst        dstv1c
043200110412     c                   clear                   sgiv1c
043300110412     c                   clear                   ddtv1c
043400110412     c                   clear                   dstv1c
043500110406     C                   Z-ADD     evdVOC        KVDVOC
043600110406     C                   Z-ADD     evdPRG        KVDPRG
043700110406     C                   MOVEL     evdTTV        KVDTTV
043800110406     C                   MOVEL     evdFT1        KVDFT1
043900110406     C                   MOVEL     evdFT2        KVDFT2
044000110406     C                   MOVEL     evdCLI        KVDCLI
044100110406     C                   MOVEL     evdCTR        KVDCTR
044200110406     C                   MOVEL     evdCTS        KVDCTS
044300131128     C                   movel     evdCAd        KVDCAd
044400131128     C                   movel     evdnaz        KVDnaz
044500131128     C     KE2EVD        SETLL     ECEVD06L
044600110412DO  2C                   DO        *hival
044700131128     C     KE2EVD        READE(N)  ECEVD06L
044800131128     c                   if        %eof(ecevd06l)
044900110406     c                   leave
045000110406     c                   end
045100110412     c                   if        ddtv1c <> 0
045200110406IF  3C     £EVDATB       IFne      *BLANKS
045300110406     c                   iter
045400110406     c                   end
045500110406IF  5C     DDTV1C        IFgt      £EVDDST
045600110406     c                   iter
045700110406     c                   end
045800110406IF  6C     DSTV1C        IFLT      £EVDDDT
045900110406     c                   iter
046000110406     c                   end
046100110412IF  7C     sgiv1c        IFEQ      £evdSGI
046200000000     C                   MOVEL     '1'           $ERR
046300000000     C                   MOVEL     MSG(27)       $MSG
046400110412     C                   EXSR      fine
046500110412     c                   leavesr
046600940725     C* APPENA TROVO L'ERRORE ESCO DAL CICLO
046700940725E   7C                   ENDIF
046800110406E   7C                   ENDIF
046900110412     c                   eval      sgiv1c = £evdsgi
047000110412     c                   eval      ddtv1c = £evdddt
047100110406     c                   eval      dstv1c = £evddst
047200000000E   2C                   ENDDO
047300000000     C* LEGGO IL RECORD DI ECEVT POSIZIONANDOMI PER DATA DECORRENZA
047400110406     C                   Z-ADD     evdVOC        KVTVOC
047500110406     C                   Z-ADD     evdPRG        KVTPRG
047600000000     C     KEYEVT        SETLL     ECEVT01L
047700110406     C     evdVOC        READE     ECEVT01L                               99
047800000000     C* SE LA DATA SCADENZA LETTA (DA ECEVT) E' INFERIORE ALLA SCADENZA A VIDEO
047900110412     C  N99evdddt        IFGE      EVTDDV
048000110412     C     evddst        ANDLE     EVTDSV
048100000000     C                   ELSE
048200000000     C                   MOVEL     '1'           $ERR
048300000000     C                   MOVEL     MSG(39)       $MSG
048400110412     C                   EXSR      fine
048500110412     c                   leavesr
048600110412     C                   ENDIF
048700000000     C*--------------------
048800000000     C* SCAGLIONE SUPERIORE TARIFFA
048900000000     C*--------------------
049000110412     C     evttiv        IFne      ' '
049100110412     C     evdSGF        andle     evdSGI
049200000000     C                   MOVEL     '1'           $ERR
049300000000     C                   MOVEL     MSG(13)       $MSG
049400110412     C                   EXSR      fine
049500110412     c                   leavesr
049600110412     C                   ENDIF
049700000000     C*--------------------
049800000000     C* IMPORTO UNIT. TARIFFA
049900940725     C* LO ACCETTO SE TIPO "QUANTITA'" O "IMPORTO FISSO" (DS2R)
050000000000     C*--------------------
050100110419     C     §2RTVA        IFeq      'P'
050200110412     C     evtTIV        ifne      *BLANKS
050300110412     C     evdITR        andgt     *ZEROS
050400940726     C                   MOVEL     '1'           $ERR
050500940726     C                   MOVEL     MSG(25)       $MSG
050600110412     C                   EXSR      fine
050700110412     c                   leavesr
050800110412     C                   ENDIF
050900110412     c                   else
051000000000     C*--------------------
051100000000     C* PERCENTUALE DA APPLICARE  (E' IN ALTERNATIVA ALL'IMPORTO UNIT.)
051200000000     C* SE NELLA VOCE NON C'E' "TIPO IMPONIBILE" NON DEVE ESSERCI LA %
051300000000     C*--------------------
051400110412     C     evdPER        IFGT      100
051500000000     C                   MOVEL     '1'           $ERR
051600000000     C                   MOVEL     MSG(41)       $MSG
051700110412     C                   EXSR      fine
051800110412     c                   leavesr
051900110412     C                   ENDIF
052000000000     C*--------------------
052100000000     C* IMPORTO MINIMO TARIFFA
052200000000     C*--------------------
052300940725     C* ACCETTO SOLO SE C'E' LA PERCENTUALE, ALTRIMENTI VUOTO
052400110412     C     evdPER        IFEQ      *ZERO
052500110412     C     evdIMI        andne     *ZERO
052600000000     C                   MOVEL     '1'           $ERR
052700000000     C                   MOVEL     MSG(32)       $MSG
052800110412     C                   EXSR      fine
052900110412     c                   leavesr
053000110412     C                   ENDIF
053100000000     C*--------------------
053200000000     C* IMPORTO MASSIMO TARIFFA
053300000000     C*--------------------
053400940725     C* ACCETTO SOLO SE C'E' LA PERCENTUALE, ALTRIMENTI VUOTO
053500110412     C     evdPER        IFEQ      *ZERO
053600110412     C     evdIMA        IFNE      *ZERO
053700000000     C                   MOVEL     '1'           $ERR
053800000000     C                   MOVEL     MSG(32)       $MSG
053900110412     C                   EXSR      fine
054000110412     c                   leavesr
054100110412     C                   ENDIF
054200110412     C                   ELSE
054300110412     C     evdIMA        IFLT      evdIMI
054400940725     C                   MOVEL     '1'           $ERR
054500940725     C                   MOVEL     MSG(36)       $MSG
054600110412     C                   EXSR      fine
054700110412     c                   leavesr
054800110412     C                   ENDIF
054900000000     C*--------------------
055000000000     C* SEGNO IMPORTO TARIFFA
055100000000     C*--------------------
055200110406IF  2C     evdSEG        IFEQ      *BLANKS
055300000000     C                   MOVEL     '1'           $ERR
055400000000     C                   MOVEL     MSG(33)       $MSG
055500110412     C                   EXSR      fine
055600110412     c                   leavesr
055700100422     c                   else
055800100422     c* se maggiorazione con voci da isolare le tariffe devono essere
055900100422     c* tutte positive o tutte negative a seconda del flag Applica S/N
056000100422     c                   if        flgsegtar = '1'
056100110406     C                   Z-ADD     evdVOC        KVTVOC
056200110406     C                   Z-ADD     evdPRG        KVTPRG
056300100422     c     keyevt        setll     ecevm01l
056400100422     c                   do        *hival
056500100422     c     keyevt        reade     ecevm01l
056600100422     c                   if        %eof(ecevm01l)
056700100422     c                   leave
056800100422     c                   end
056900110419     c*                  if        (evmapp = 'S' and evdseg = '+') or
057000110419     c*                            (evmapp = 'N' and evdseg = '-')
057100110419     C*                  MOVEL     '1'           $ERR
057200110419     C*                  MOVEL     MSG(42)       $MSG
057300110419     C*                  EXSR      fine
057400110419     c*                  leavesr
057500110419     c*                  end
057600100422     c                   enddo
057700100422     c                   end
057800110412     C                   ENDIF
057900110412     C                   ENDIF
058000110412     C                   ENDIF
058100000000     C*--------------------
058200000000     C* RAPPORTO PESO VOLUME
058300940725     C* LO ACCETTO SOLO SE NELLA VOCE LO CONSENTE IL TIPO RIFERIMENTO
058400000000     C*--------------------
058500110412     C     §2RRPV        IFEQ      'N'
058600110412     C     evdRPV        IFNE      *ZERO
058700000000     C                   MOVEL     '1'           $ERR
058800000000     C                   MOVEL     MSG(15)       $MSG
058900110412     C                   EXSR      fine
059000110412     c                   leavesr
059100110412     C                   ENDIF
059200110412     C                   ELSE
059300110412     C     EVTSPT        IFGT      '1'
059400110406     C     evdRPV        ANDEQ     *ZEROS
059500961202     C                   MOVEL     '1'           $ERR
059600961202     C                   MOVEL     MSG(26)       $MSG
059700110412     C                   EXSR      fine
059800110412     c                   leavesr
059900110412     C                   ENDIF
060000110412     C                   ENDIF
060100100331     C*--------------------
060200100331     C* SE VOCE DA ISOLARE PER MAGGIORAZ. si deve inserire solo cartello
060300100331     c* 100%
060400100331     C*--------------------
060500100422     C                   IF        evtvrv = '26' and
060600110406     c                             (evdttv <> 'C' or evdper <> 100
060700110406     c                             or evdseg <> '+')
060800100331     C                   MOVEL     '1'           $ERR
060900100331     C                   MOVEL     MSG(9)        $MSG
061000110412     C                   EXSR      fine
061100110412     c                   leavesr
061200100331     c                   end
061300000000     C*
061400000000     C                   ENDSR
061500970627     C*------------------------------------------------------------------------*
061600110412     C* fine - stampa
061700970627     C*------------------------------------------------------------------------*
061800110412     C     fine          BEGSR
061900970627     C*
062000110412     c                   if        *inof
062100110412     c                   except    testa
062200110412     c                   setoff                                       of
062300110412     c                   end
062400110418     c                   except    riga
062500110418     c                   eval      $err = '0'
062600110412     C*
062700110412     C                   ENDSR
062800110412     C*------------------------------------------------------------------------*
062900110412     C* CHKCAP - CONTROLLA IL CAP
063000110412     C*------------------------------------------------------------------------*
063100110412     C     CHKCAP        BEGSR
063200110412     C*
063300970627     C     *LIKE         DEFINE    I95NAR        WNAR
063400970627     C     *LIKE         DEFINE    I95CAP        WCAP
063500970627     C     *LIKE         DEFINE    I95DAT        WDAT
063600140522     C     *LIKE         DEFINE    O95VER        Wver
063700970627     C*
063800970627     C* CONTROLLA IL CAP
063900970627     C                   CLEAR                   DSSI95
064000970627     C                   MOVEL     *BLANKS       I95TLA
064100970627     C                   MOVEL     '3'           I95TCN
064200970627     C                   MOVEL     WNAR          I95NAR
064300970627     C                   MOVEL     WCAP          I95CAP
064400970627     C                   Z-ADD     WDAT          I95DAT
064500970627     C                   CALL      'TISI95R'
064600970627     C                   PARM                    DSSI95
064700970627IF  1C     O95ERR        IFNE      *BLANKS
064800140522     c* verifico se è un cap fittizio
064900140522     c     kazcpc        chain     azcpc01l
065000140522     c                   if        not %found(azcpc01l)
065100140522     C                   MOVEL     '1'           $ERR
065200140522E   1C                   ENDIF
065300970627E   1C                   ENDIF
065400970627     C*
065500970627     C                   ENDSR
065600100422     C*------------------------------------------------------------------------*
065700100422     C* CHKCTR - CONTROLLA CODICE TARIFFA
065800100422     C*          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
065900100422     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
066000100422     C*------------------------------------------------------------------------*
066100100422     C     CHKCTR        BEGSR
066200000000     C*
066300950719IF  2C     $CTR          IFGE      *ZEROS
066400110406     C                   MOVEL     evdCLI        KTAKSC
066500000000     C                   MOVEL     $CTR          KTACTR
066600000000     C*
066700941128     C     KEYTAM        CHAIN     TNTAM01L                           99
066800940722     C* SE INESISTENTE VA BENE LO STESSO: EVIDENZIO PERO' LA COSA
066900000000IF  4C     *IN99         IFEQ      *ON
067000110418     C     $CTR          ANDNE     '999'
067100110412     C                   MOVEL     '1'           $ERR
067200000000E   4C                   ENDIF
067300000000     C*
067400000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
067500000000X   3C                   ELSE
067600000000     C                   MOVEL     '1'           $ERR
067700000000E   3C                   ENDIF
067800000000     C*
067900000000     C                   ENDSR
068000000000     C*------------------------------------------------------------------------*
068100000000     C* CHKDAT - CONTROLLA LA DATA
068200000000     C*          INPUT --> $DAT:  DATA
068300000000     C*------------------------------------------------------------------------*
068400000000     C     CHKDAT        BEGSR
068500000000     C*
068600000000     C* OBBLIGATORIA
068700000000IF  1C     $DAT          IFEQ      *ZEROS
068800000000     C                   MOVEL     '1'           $ERR
068900000000     C                   MOVEL     MSG(8)        $MSG
069000000000X   1C                   ELSE
069100000000     C* CORRETTA
069200110418     C                   Z-ADD     *zeros        G02DAT
069300110418     C                   Z-ADD     $DAT          G02INV
069400110418     C                   MOVEL     '3'           G02ERR
069500000000     C                   CALL      'XSRDA8'
069600000000     C                   PARM                    WLBDAT
069700000000     C*
069800000000IF  2C     G02ERR        IFEQ      '1'
069900000000     C                   MOVEL     '1'           $ERR
070000000000     C                   MOVEL     MSG(8)        $MSG
070100000000X   2C                   ELSE
070200000000     C                   Z-ADD     G02DAT        $DAT
070300000000E   2C                   ENDIF
070400000000E   1C                   ENDIF
070500000000     C*
070600000000     C                   ENDSR
070700000000     C*------------------------------------------------------------------------*
070800000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
070900000000     C*          INPUT --> §COD: CODICE TABELLA
071000000000     C*                    $KEY: CODICE ELEMENTO
071100000000     C*------------------------------------------------------------------------*
071200000000     C     CHKTAB        BEGSR
071300000000     C*
071400110412     C     *LIKE         DEFINE    tblcod        §cod
071500110412     C     *LIKE         DEFINE    tblkey        §key
071600020923     C                   MOVEL     1             KTBKUT
071700000000     C                   MOVEL     §COD          KTBCOD
071800000000     C                   MOVE      *BLANKS       KTBKEY
071900000000     C                   MOVEL     §KEY          KTBKEY
072000000000     C     KEYTAB        CHAIN     TABEL00F                           99
072100000000     C     *IN99         IFEQ      '1'
072200000000     C     TBLFLG        ORNE      *BLANKS
072300000000     C                   MOVEL     '1'           $ERR
072400000000     C                   ENDIF
072500000000     C*
072600000000     C                   ENDSR
072700970627     C*------------------------------------------------------------------------*
072800970627     C* FINSR - OPERAZIONI FINALI
072900970627     C*------------------------------------------------------------------------*
073000970627     C     FINSR         BEGSR
073100970627     C*
073200971020     C* TISI95R
073300970704     C                   CLEAR                   DSSI95
073400970627     C                   MOVEL     'C'           I95TLA
073500970627     C                   CALL      'TISI95R'
073600970627     C                   PARM                    DSSI95
073700971020     C* FNLV55R
073800971020     C                   CLEAR                   DSLV55
073900971020     C                   MOVEL     'C'           D55TLA
074000971020     C                   CALL      'FNLV55R'
074100971020     C                   PARM                    DSLV55
074200000727     C* TIBS69R
074300000727     C                   CLEAR                   BS69DS
074400000727     C                   CLEAR                   ACODS
074500000727     C                   CLEAR                   INDDS
074600000727     C                   CLEAR                   CLPDS
074700000727     C                   CLEAR                   CLSDS
074800000727     C                   MOVEL     'C'           I69TLA
074900000727     C                   MOVEL     KNSIF         I69SIF
075000000727     C                   CALL      'TIBS69R'
075100000727     C                   PARM                    BS69DS
075200000727     C                   PARM                    ACODS
075300000727     C                   PARM                    INDDS
075400000727     C                   PARM                    CLPDS
075500000727     C                   PARM                    CLSDS
075600970627     C*
075700970627     C                   ENDSR
075800020923     C*-------------------------------------------------------------------------
075900020923     C* REPERISCE I DATI UTENTE
076000020923     C*-------------------------------------------------------------------------
076100020923     C     REPDATIUTE    BEGSR
076200020923     C*
076300020923     C* INIZIALIZZA VARIABILI DI WRK
076400020923     C                   CLEAR                   TIBS34DS
076500020923     C                   CLEAR                   AZUTEDS
076600020923     C                   CLEAR                   DDATIUTE
076700020923     C*
076800020923     C     *DTAARA       DEFINE    §azute        azuteds
076900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
077000020923     C                   IN(E)     *DTAARA
077100020923if  1C                   IF        %Error
077200020923     c                   EVAL      I34Tla = 'L'
077300020923     C                   CALL      'TIBS34R'
077400020923     C                   PARM                    Tibs34Ds
077500020923     C                   IN        *DTAARA
077600110412     C                   ENDIF
077700020923     C                   ENDSR
077800000000     C*------------------------------------------------------------------------*
077900000000     C*  *INZSR - ROUTINE INIZIALE
078000000000     C*------------------------------------------------------------------------*
078100000000     C     *INZSR        BEGSR
078200000000     C*
078300000000     C* RICEVIMENTO PARAMETRI
078400000000     C     *ENTRY        PLIST
078500000000     C                   PARM                    KPJBA
078600110412     C                   MOVEL     KPJBU         lib              10
078700110412     c                   seton                                        of
078800000000     C*
078900000000     C* DEFINIZIONE VARIABILI
079000000000     C     *LIKE         DEFINE    ORGFIL        KRGFIL
079100970627     C     *LIKE         DEFINE    TAMKSC        KTAKSC
079200000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
079300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
079400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
079500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
079600000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC
079700000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
079800000000     C     *LIKE         DEFINE    EVDTTV        KVDTTV
079900000000     C     *LIKE         DEFINE    EVDFT1        KVDFT1
080000000000     C     *LIKE         DEFINE    EVDFT2        KVDFT2
080100000000     C     *LIKE         DEFINE    EVDCLI        KVDCLI
080200000000     C     *LIKE         DEFINE    EVDCTR        KVDCTR
080300000000     C     *LIKE         DEFINE    EVDCTS        KVDCTS
080400131128     C     *LIKE         DEFINE    EVDnaz        KVDnaz
080500131128     C     *LIKE         DEFINE    EVDCAd        KVDCAd
080600000000     C     *LIKE         DEFINE    EVDDDT        KVDDDT
080700000000     C     *LIKE         DEFINE    EVDSGI        KVDSGI
080800000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
080900000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
081000000000     C*
081100131128     C* CHIAVE LETTURA ECEVD06L - COMPLETA
081200000000     C     KEYEVD        KLIST
081300000000     C                   KFLD                    KVDVOC
081400000000     C                   KFLD                    KVDPRG
081500000000     C                   KFLD                    KVDTTV
081600000000     C                   KFLD                    KVDFT1
081700000000     C                   KFLD                    KVDFT2
081800000000     C                   KFLD                    KVDCLI
081900000000     C                   KFLD                    KVDCTR
082000000000     C                   KFLD                    KVDCTS
082100131128     C                   KFLD                    KVDnaz
082200131128     C                   KFLD                    KVDcad
082300000000     C                   KFLD                    KVDDDT
082400000000     C                   KFLD                    KVDSGI
082500000000     C*
082600131128     C* CHIAVE LETTURA ECEVD06L - PARZIALE
082700000000     C     KE2EVD        KLIST
082800000000     C                   KFLD                    KVDVOC
082900000000     C                   KFLD                    KVDPRG
083000000000     C                   KFLD                    KVDTTV
083100000000     C                   KFLD                    KVDFT1
083200000000     C                   KFLD                    KVDFT2
083300000000     C                   KFLD                    KVDCLI
083400000000     C                   KFLD                    KVDCTR
083500000000     C                   KFLD                    KVDCTS
083600131128     C                   KFLD                    KVDnaz
083700131128     C                   KFLD                    KVDcad
083800000000     C*
083900131128     C* CHIAVE LETTURA ECEVD06L - COMPLETA
084000000000     C     KE3EVD        KLIST
084100000000     C                   KFLD                    KVDVOC
084200000000     C                   KFLD                    KVDPRG
084300000000     C                   KFLD                    KVDTTV
084400000000     C*
084500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
084600000000     C     KEYTAB        KLIST
084700000000     C                   KFLD                    KTBKUT
084800000000     C                   KFLD                    KTBCOD
084900000000     C                   KFLD                    KTBKEY
085000000000     C*
085100941128     C* CHIAVE LETTURA TNTAM01L - PARZIALE
085200000000     C     KEYTAM        KLIST
085300000000     C                   KFLD                    KTAKSC
085400000000     C                   KFLD                    KTACTR
085500000000     C*
085600000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
085700000000     C     KEYEVT        KLIST
085800000000     C                   KFLD                    KVTVOC
085900940728     C                   KFLD                    KVTPRG
086000971020     C*
086100140522     C* CHIAVE LETTURA azcpc01L - COMPLETA
086200140522     C     Kazcpc        KLIST
086300140522     C                   KFLD                    wver
086400140522     C                   KFLD                    wnar
086500140522     C                   KFLD                    wcap
086600140522      *
086700140522     c* reperisco la versione del cappario
086800140522     C                   CLEAR                   wver
086900140522     C                   CLEAR                   DSSI95
087000140522     C                   MOVEL     '3'           I95TCN
087100140522     C                   MOVEL(p)  '40100'       I95CAP
087200140522     C                   Z-ADD     WDAT          I95DAT
087300140522     C                   CALL      'TISI95R'
087400140522     C                   PARM                    DSSI95
087500140522IF  1C     O95ERR        IFeq      *BLANKS
087600140522     C                   MOVEL     o95ver        wver
087700140522E   1C                   ENDIF
087800971020     C* REPERISCE LA DATA CORRENTE
087900971020     C                   TIME                    N14              14 0
088000971020     C                   MOVE      N14           N8                8 0
088100110419     C                   MOVEl     N14           N6                6 0
088200971020     C                   Z-ADD     N8            G02DAT
088300971020     C                   Z-ADD     *ZEROS        G02INV
088400971020     C                   MOVEL     '0'           G02ERR
088500971020     C                   CALL      'XSRDA8'
088600971020     C                   PARM                    WLBDAT
088700971020     C                   Z-ADD     G02INV        DATCOR            8 0
088800000000     C*
088900000000     C                   ENDSR
089000110419     oprtf198   e            testa             1
089100110505     o                                          100 'Controllo tariffe C/E'
089200110412     o                       lib                + 1
089300110419     o                       n8                 +10 '  /  /    '
089400110419     o                       n6                 + 2 '  :  :  '
089500110419     o                       page               198
089600110419     o          e            testa             3  4
089700110419     o                                          + 0 'Voce'
089800110419     o                                            9 'T'
089900110419     o                                           17 'Filiale'
090000110419     o                                           25 'Cliente'
090100110419     o                                           29 'Tar'
090200131203     o                                           36 'Naz'
090300131203     o                                           42 'C.A.P'
090400131203     o                                           59 'Decorrenza'
090500131203     o                                           70 'Scadenza'
090600131203     o                                           85 'Scaglione I.'
090700131203     o                                          100 'Scaglione F.'
090800131203     o                                          115 'I. Unitario'
090900131203     o                                          130 'I. Minimo'
091000131203     o                                          145 'I. Massimo'
091100131203     o                                          147 'S'
091200131203     o                                          154 '%'
091300131203     o                                          159 'RPV'
091400131203     o                                          170 'Inserimen.'
091500131203     o                                          181 'Modifica'
091600131203     o                                          194 'Trasmissione'
091700131203     o                                          196 'A'
091800110418     o          e            riga        1
091900110418     o                       EVDVOC              +0
092000110418     o                       EVDPRG              +1
092100110418     o                       EVDTTV              +1
092200110418     o                       EVDFT1              +1
092300110418     o                       EVDFT2              +1
092400110418     o                       EVDCLI              +1
092500110418     o                       EVDCTR              +1
092600110418     o                       EVDCTS              +1
092700131203     o                       EVDnaz              +1
092800131203     o                       EVDcad              +1
092900110418     o                       EVDDDT              +1 '    /  /  '
093000110418     o                       EVDDST              +1 '    /  /  '
093100110418     o                       EVDSGI        4     +1
093200110418     o                       EVDSGF        4     +1
093300110418     o                       EVDITR        4     +1
093400110418     o                       EVDIMI        4     +1
093500110418     o                       EVDIMA        4     +1
093600110418     o                       EVDSEG              +1
093700110418     o                       EVDPER        4     +1
093800110418     o                       EVDRPV        4     +1
093900110418     o                       EVDDIR              +1 '    /  /  '
094000110418     o                       EVDDUV              +1 '    /  /  '
094100110418     o                       EVDFTR              +1
094200110418     o                       EVDDTR              +1 '    /  /  '
094300110418     o                       EVDATB              +1
094400110418     o          e            riga        1
094500110412     o                       $msg                78
094600981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
094700110406                                                                               1
094800110406                                                                               2
094900110406                                                                               3
095000110406                                                                               4
095100110406                                                                               5
095200110406                                                                               6
095300110406Immettere codice voce esistente                                                7
095400110406Immettere data formalmente valida                                              8
095500110406La tariffa per voce isolamento maggioraz. deve essere CARTELLO          100%   9
095600110406Immettere codice C.A.P. esistente e non annullato                             10
095700110406Tariffa già esistente                                                         11
095800110420Immettere codice tariffa valido
095900110406Immettere scaglione tariffa                                                   13
096000110406Immettere codice filiale esistente                                            14
096100110406Non immettere il rapporto peso volume per il tipo riferimento          voce   15
096200110406Immettere codice tassazione esistente e non annullato                         16
096300110406                                                                              17
096400110406                                                                              18
096500110406                                                                              19
096600110406                                                                              20
096700110406Richiesta funzione per voce già esistente                                     21
096800110406                                                                              22
096900110406La data scadenza deve essere maggiore o uguale della decorrenza               23
097000110406Per il Tipo Riferimento Voce non e' possibile aggiungere piu' voci            24
097100110406Immettere importo unitario solo se tipo tariffa a Quantità/Imp fisso          25
097200110406Rapporto peso volume obbligatorio                                             26
097300110406La validità della tariffa si sovrappone ad una già in decorrenza              27
097400110406Immettere filiali non uguali                                                  28
097500110406                                                                              29
097600110406Immettere codice cliente esistente e non annullato                            30
097700110406La tariffa x voce isolamento maggior. deve avere scadenza 31/12/2039          31
097800110406Non immettere importo se non presente la percentuale                          32
097900110406Immettere il segno per la tariffa                                             33
098000110406Se una filiale e' 1° livello l'altra deve appartenere all'area                34
098100110406Immettere codice C.A.P. esistente e non annullato                             35
098200110406Immettere importo minimo inferiore al massimo                                 36
098300110412                                                                              37
098400110406Per il riferimento tariffa, non si puo' immettere la filiale                  38
098500110406Il periodo immesso non e' compreso in quello della voce                       39
098600110406Nella voce non vi e' alcun riferimento per il tipo tariffa                    40
098700110406Immettere una percentuale valida                                              41
098800110406La voce di isolamento ha il flag Applica incongruente con il segno            42
098900110406
099000940831* CASO PARTICOLARE *                                                          44
099100140522*** INESISTENTE ***                                                           45
099200140522TREC64R- Scaglione incongruente con scaglioni esistenti                       46
099300140522TREC64R- Imserire un CAP generico correttamente                               47
099400140522TREC64R- Codice Nazione Inesistente                                           48
