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
017600141203     C* SE E' MAGGIORAZIONE NON ACCETTO TIPO TARIFFA se non abilitata
017700141203IF  2C     evtVRV        IFNE      *BLANKS
017800141203     C     evtTVO        ANDEQ     'M'
017900141203     C     §2rmag        ANDEQ     ' '
018000141203     C                   MOVEL     '1'           $ERR
018100141203     C                   MOVEL     MSG(37)       $MSG
018200141203     C                   EXSR      fine
018300141203     c                   leavesr
018400141203E   2C                   ENDIF
018500141203     c* per maggiorazioni con tipo tariffa forzo la tipologia a percentuale
018600141203     c* 0 % nessun calcolo con tipo tariffa - 100 % calcolo con tipo tariffa
018700141203     c                   if        evttvo = 'M'  and
018800141203     C                             evtvrv <> *BLANKS
018900141203     c                   if        evdper <> 100
019000141203     c                             and evdper <> 0
019100141203     c                             or evdseg <> '+'
019200141203     C                   MOVEL     '1'           $ERR
019300141203     C                   MOVEL     MSG(29)       $MSG
019400141203     C                   EXSR      fine
019500141203     c                   leavesr
019600141203E   2C                   ENDIF
019700141203E   2C                   ENDIF
019800000000     C* NELLA DS2R C'E' UN FLAG PER CONSENTIRE UN SOLO, O PIU', SCAGLIONE
019900000000     C* CONTROLLO DA FARE SOLO IN INSERIMENTO
020000110412     C     §2RPIU        IFNE      'S'
020100110405     C                   MOVE      evdVOC        KVDVOC
020200110405     C                   MOVE      evdPRG        KVDPRG
020300110405     C                   MOVE      evdTTV        KVDTTV
020400131128     C     KE3EVD        SETLL     ECEVD06l
020500131128     C     KE3EVD        READE     ECEVD06l                               99
020600110412     C     *IN99         DOWEQ     *OFF
020700110412     C     £EVDATB       IFEQ      *BLANKS
020800110412     C     £EVDSGI       andne     evdSGI
020900000000     C                   MOVEL     '1'           $ERR
021000000000     C                   MOVEL     MSG(24)       $MSG
021100110406     C                   EXSR      fine
021200110412     c                   leavesr
021300110412     C                   ENDIF
021400131128     C  N99KE3EVD        READE     ECEVD06L                               99
021500110412     C                   ENDDO
021600110412     c                   ENDIF
021700000000     C* TIPO TARIFFA
021800000000     C                   MOVEL     '2S'          §COD
021900000000     C                   MOVEL     *BLANKS       §KEY
022000110406     C                   MOVEL     evdTTV        §KEY
022100000000     C                   EXSR      CHKTAB
022200000000     C*?    V O C E    D I    C L I E N T E    ?
022300110412     C     evdTTV        IFEQ      'K'
022400000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X CLIENTE NON POSSO
022500000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'K'
022600000000     C     EVTRKI        IFEQ      *BLANKS
022700000000     C     EVTRKE        ANDEQ     *BLANKS
022800000000     C                   MOVEL     '1'           $ERR
022900000000     C                   MOVEL     MSG(40)       $MSG
023000110406     C                   EXSR      fine
023100110412     c                   leavesr
023200000000     C                   ENDIF
023300000727     C* CONTROLLA VALIDITA' CODICE
023400000727     C                   CLEAR                   BS69DS
023500000727     C                   CLEAR                   ACODS
023600000727     C                   MOVEL     KNSIF         I69SIF
023700110406     C                   MOVEL     evdCLI        I69KAC
023800000727     C                   CALL      'TIBS69R'
023900000727     C                   PARM                    BS69DS
024000000727     C                   PARM                    ACODS
024100000727     C                   PARM                    INDDS
024200000727     C                   PARM                    CLPDS
024300000727     C                   PARM                    CLSDS
024400110406     c                   if        o69err = '1'
024500110406     C                   MOVEL     '1'           $ERR
024600110406     C                   MOVEL     MSG(30)       $MSG
024700110406     C                   EXSR      fine
024800110412     c                   leavesr
024900110406     c                   end
025000000000     C*--------------------
025100000000     C* TARIFFA CLIENTE
025200000000     C*--------------------
025300110406     C                   MOVEL     evdCTR        $CTR              3
025400000000     C                   EXSR      CHKCTR
025500940722     C* ERRORE (CARATTERI ALFABETICI)
025600110412     C     $ERR          IFEQ      '1'
025700110412     C                   MOVEL     MSG(12)       $MSG
025800110406     C                   EXSR      fine
025900110412     c                   leavesr
026000110412     C                   ENDIF
026100110418     C                   ENDIF
026200110412     C*?    V O C E    D I    C. A. P.    ?
026300110412     C     evdTTV        IFEQ      'P'
026400000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X C.A.P.  NON POSSO
026500000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'P'
026600000000     C     EVTRPI        IFEQ      *BLANKS
026700000000     C     EVTRPE        ANDEQ     *BLANKS
026800000000     C                   MOVEL     '1'           $ERR
026900000000     C                   MOVEL     MSG(40)       $MSG
027000110406     C                   EXSR      fine
027100110412     c                   leavesr
027200110412     C                   ENDIF
027300140522     C*--------------------
027400140522     C* CODICE C.A.P.
027500140522     C*--------------------
027600140522IF   C     $ERR          IFEQ      '0'
027700140522      **   essendo ITALIA definita sulla tab 15 come Blk
027800140522      **   deve sempre testare
027900140522     C                   MOVEL     1             KTBKUT
028000140522     C                   MOVEL     '15'          KTBCOD
028100140522     C                   MOVEl     evdnaz        ktbkey
028200140522     C                   CLEAR                   DS15
028300140522     C     KEYTAB        CHAIN     TABEL00F                           99
028400140522     C                   IF        %found(tabel00f) and
028500140522     C                             tblflg = *BLANKS
028600140522     C                   MOVEL     TBLUNI        DS15
028700140522X    C                   ElsE
028800140522     C                   MOVEL     '1'           $ERR
028900140522     C                   MOVEL     MSG(48)       $MSG
029000140522     C                   EXSR      fine
029100140523     c                   leavesr
029200140522E    C                   ENDIF
029300140522E    C                   ENDIF
029400140522      * x Italia
029500140522IF   C     $ERR          IFEQ      '0'
029600140522IF   C                   IF        evdnaz = *blank and EVTRPI ='PP'
029700140522     c                               or
029800140522     C                             evdnaz <>*blank and EVTRPE ='PP'
029900140522     c*
030000140522     c                   if         %len(%trim(%subst(evdCAD:§15pcf)))
030100140522     c                                  > §15ecf
030200140522     C                   MOVEL     '1'           $ERR
030300140522     C                   MOVEL     MSG(47)       $MSG
030400140522     C                   EXSR      fine
030500140523     c                   leavesr
030600140522     c                   endif
030700140522E    C                   endIF
030800140522E    C                   endIF
030900140522      *
031000140522     C     $ERR          IfeQ      '0'
031100140522     C                   MOVEL(p)  evdNAZ        WNAR
031200140522     C                   MOVEL(P)  evdCAd        WCAP
031300140522     C                   Z-ADD     *ZEROS        WDAT
031400140522     C                   EXSR      CHKCAP
031500140522IF   C     $ERR          IFEQ      '1'
031600140522     C                   MOVEL     MSG(10)       $MSG
031700140522     C                   EXSR      fine
031800140523     c                   leavesr
031900140522E    C                   ENDIF
032000140522E    C                   ENDIF
032100110412     C                   ENDIF
032200000000     C*?    V O C E    D I    T A S S A Z I O N E    ?
032300110412     C     evdTTV        IFEQ      'T'
032400000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X TASSAZIONE NON POSSO
032500000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'T'
032600000000     C     EVTRTI        IFEQ      *BLANKS
032700000000     C     EVTRTE        ANDEQ     *BLANKS
032800000000     C                   MOVEL     '1'           $ERR
032900000000     C                   MOVEL     MSG(40)       $MSG
033000110406     C                   EXSR      fine
033100110412     c                   leavesr
033200000000     C                   ENDIF
033300000000     C*--------------------
033400000000     C* CODICE TASSAZIONE
033500000000     C*--------------------
033600000000     C                   MOVE      'CT'          §COD
033700000000     C                   MOVEL     *BLANKS       §KEY
033800110406     C                   MOVEL     evdCTS        §KEY
033900000000     C                   EXSR      CHKTAB
034000110412     C     $ERR          IFEQ      '1'
034100000000     C                   MOVEL     MSG(16)       $MSG
034200110406     C                   EXSR      fine
034300110412     c                   leavesr
034400110412     C                   ENDIF
034500110412     C                   ENDIF
034600110412     C*?    V O C E    D I    F I L I A L E    ?
034700110412     C     evdTTV        IFEQ      'F'
034800000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X FILIALE NON POSSO
034900000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'F'
035000000000     C     EVTRFI        IFEQ      *BLANKS
035100000000     C     EVTRFE        ANDEQ     *BLANKS
035200000000     C                   MOVEL     '1'           $ERR
035300000000     C                   MOVEL     MSG(40)       $MSG
035400110412     C                   EXSR      fine
035500110412     c                   leavesr
035600000000     C                   ENDIF
035700000000     C*--------------------
035800000000     C* CODICE FILIALE/TERMINAL
035900000000     C*--------------------
036000000000     C* AGGANCIO LA TABELLA 2Q CON IL RIFERIMENTO TARIFFA (ITALIA/ESTERO)
036100000000     C* E' SUFFICIENTE FARLO SOLO PER L'ITALIA, IN QUANTO I FLAG PER L'ITALIA E
036200000000     C* PER L'ESTERO DEVONO ESSERE UGUALI
036300020923     C                   MOVEL     1             KTBKUT
036400000000     C                   MOVEL     '2Q'          KTBCOD
036500000000     C                   MOVE      *BLANKS       KTBKEY
036600940728IF  4C     EVTRFI        IFNE      *BLANKS
036700000000     C                   MOVEL     EVTRFI        KTBKEY
036800940728X   4C                   ELSE
036900000000     C                   MOVEL     EVTRFE        KTBKEY
037000940728E   4C                   ENDIF
037100000000     C     KEYTAB        CHAIN     TABEL00F                           99
037200940728IF  4C     *IN99         IFEQ      '1'
037300000000     C     TBLFLG        ORNE      *BLANKS
037400000000     C                   CLEAR                   DS2Q
037500940728X   4C                   ELSE
037600000000     C                   MOVEL     TBLUNI        DS2Q
037700940728E   4C                   ENDIF
037800000000     C*
037900110412     C     evdFT1        IFGE      *ZERO
038000110412     C     evdft1        CHAIN     AZORG01L
038100110418     C                   IF        not %found(azorg01l)
038200000000     C                   MOVEL     '1'           $ERR
038300000000     C                   MOVEL     MSG(14)       $MSG
038400110412     C                   EXSR      fine
038500110412     c                   leavesr
038600110412     C                   ENDIF
038700110412     C                   ELSE
038800000000     C                   MOVEL     '1'           $ERR
038900000000     C                   MOVEL     MSG(14)       $MSG
039000110412     C                   EXSR      fine
039100110412     c                   leavesr
039200110412     C                   ENDIF
039300000000     C* 2° FILIALE
039400110412     C     evdFT2        IFGT      *ZERO
039500000000     C     §2QFAR        ORNE      *BLANKS
039600110405     C     evdft2        CHAIN     AZORG01L                           65
039700110412     C     *IN65         IFEQ      *ON
039800000000     C                   MOVEL     '1'           $ERR
039900000000     C                   MOVEL     MSG(14)       $MSG
040000110412     C                   EXSR      fine
040100110412     c                   leavesr
040200110412     C                   ELSE
040300000000     C* SE NELLA 2Q NON C'E' IL FLAG §2QFAR, NON DEVE ESSERCI IL COD.FILIALE
040400110412     C     §2QFAR        IFEQ      *BLANKS
040500110406     C     evdFT2        ANDNE     *ZERO
040600000000     C                   MOVEL     '1'           $ERR
040700000000     C                   MOVEL     MSG(38)       $MSG
040800110412     C                   EXSR      fine
040900110412     c                   leavesr
041000110412     C                   ENDIF
041100110412     C                   ENDIF
041200110412     C                   ENDIF
041300110412     C                   ENDIF
041400000000     C*?    CAMPI  COMUNI  A  TUTTI  I  TIPI  DI  TARIFFA     ?
041500000000     C*--------------------
041600000000     C* DATA DECORRENZA (SOLO SE INSERIMENTO)
041700000000     C*--------------------
041800110406     C                   Z-ADD     evdDDT        $DAT              8 0
041900000000     C                   EXSR      CHKDAT
042000110412     C     $ERR          IFEQ      '1'
042100110412     C                   EXSR      fine
042200110412     c                   leavesr
042300110412     C                   ENDIF
042400000000     C*--------------------
042500000000     C* DATA SCADENZA
042600000000     C*--------------------
042700110406     C                   Z-ADD     evdDST        $DAT
042800000000     C                   EXSR      CHKDAT
042900110412     C     $ERR          IFEQ      '1'
043000110412     C                   EXSR      fine
043100110412     c                   leavesr
043200110412     C                   ENDIF
043300000000     C* MAGGIORE DELLA DECORRENZA
043400110412     C     evddst        IFLT      evdddt
043500000000     C                   MOVEL     '1'           $ERR
043600000000     C                   MOVEL     MSG(23)       $MSG
043700110412     C                   EXSR      fine
043800110412     c                   leavesr
043900100330     c                   else
044000110406     c* se voce da isolare non può avere una scadenza
044100110406     c                   if        evtvrv = '26' and evddst <> 20391231
044200100330     C                   MOVEL     '1'           $ERR
044300100330     C                   MOVEL     MSG(31)       $MSG
044400110412     C                   EXSR      fine
044500110412     c                   leavesr
044600000000E   2C                   ENDIF
044700100330E   2C                   ENDIF
044800000000     C*--------------------
044900000000     C* VALIDITA' VOCE PER NON AVERE PERIODI ACCAVALLATI IN ARCHIVIO
045000000000     C*--------------------
045100110412     C     *LIKE         DEFINE    evdsgi        sgiv1c
045200110412     C     *LIKE         DEFINE    evdddt        ddtv1c
045300110412     C     *LIKE         DEFINE    evddst        dstv1c
045400110412     c                   clear                   sgiv1c
045500110412     c                   clear                   ddtv1c
045600110412     c                   clear                   dstv1c
045700110406     C                   Z-ADD     evdVOC        KVDVOC
045800110406     C                   Z-ADD     evdPRG        KVDPRG
045900110406     C                   MOVEL     evdTTV        KVDTTV
046000110406     C                   MOVEL     evdFT1        KVDFT1
046100110406     C                   MOVEL     evdFT2        KVDFT2
046200110406     C                   MOVEL     evdCLI        KVDCLI
046300110406     C                   MOVEL     evdCTR        KVDCTR
046400110406     C                   MOVEL     evdCTS        KVDCTS
046500131128     C                   movel     evdCAd        KVDCAd
046600131128     C                   movel     evdnaz        KVDnaz
046700131128     C     KE2EVD        SETLL     ECEVD06L
046800110412DO  2C                   DO        *hival
046900131128     C     KE2EVD        READE(N)  ECEVD06L
047000131128     c                   if        %eof(ecevd06l)
047100110406     c                   leave
047200110406     c                   end
047300110412     c                   if        ddtv1c <> 0
047400110406IF  3C     £EVDATB       IFne      *BLANKS
047500110406     c                   iter
047600110406     c                   end
047700110406IF  5C     DDTV1C        IFgt      £EVDDST
047800110406     c                   iter
047900110406     c                   end
048000110406IF  6C     DSTV1C        IFLT      £EVDDDT
048100110406     c                   iter
048200110406     c                   end
048300110412IF  7C     sgiv1c        IFEQ      £evdSGI
048400000000     C                   MOVEL     '1'           $ERR
048500000000     C                   MOVEL     MSG(27)       $MSG
048600110412     C                   EXSR      fine
048700110412     c                   leavesr
048800940725     C* APPENA TROVO L'ERRORE ESCO DAL CICLO
048900940725E   7C                   ENDIF
049000110406E   7C                   ENDIF
049100110412     c                   eval      sgiv1c = £evdsgi
049200110412     c                   eval      ddtv1c = £evdddt
049300110406     c                   eval      dstv1c = £evddst
049400000000E   2C                   ENDDO
049500000000     C* LEGGO IL RECORD DI ECEVT POSIZIONANDOMI PER DATA DECORRENZA
049600110406     C                   Z-ADD     evdVOC        KVTVOC
049700110406     C                   Z-ADD     evdPRG        KVTPRG
049800000000     C     KEYEVT        SETLL     ECEVT01L
049900110406     C     evdVOC        READE     ECEVT01L                               99
050000000000     C* SE LA DATA SCADENZA LETTA (DA ECEVT) E' INFERIORE ALLA SCADENZA A VIDEO
050100110412     C  N99evdddt        IFGE      EVTDDV
050200110412     C     evddst        ANDLE     EVTDSV
050300000000     C                   ELSE
050400000000     C                   MOVEL     '1'           $ERR
050500000000     C                   MOVEL     MSG(39)       $MSG
050600110412     C                   EXSR      fine
050700110412     c                   leavesr
050800110412     C                   ENDIF
050900000000     C*--------------------
051000000000     C* SCAGLIONE SUPERIORE TARIFFA
051100000000     C*--------------------
051200110412     C     evttiv        IFne      ' '
051300110412     C     evdSGF        andle     evdSGI
051400000000     C                   MOVEL     '1'           $ERR
051500000000     C                   MOVEL     MSG(13)       $MSG
051600110412     C                   EXSR      fine
051700110412     c                   leavesr
051800110412     C                   ENDIF
051900000000     C*--------------------
052000000000     C* IMPORTO UNIT. TARIFFA
052100940725     C* LO ACCETTO SE TIPO "QUANTITA'" O "IMPORTO FISSO" (DS2R)
052200000000     C*--------------------
052300110419     C     §2RTVA        IFeq      'P'
052400110412     C     evtTIV        ifne      *BLANKS
052500110412     C     evdITR        andgt     *ZEROS
052600940726     C                   MOVEL     '1'           $ERR
052700940726     C                   MOVEL     MSG(25)       $MSG
052800110412     C                   EXSR      fine
052900110412     c                   leavesr
053000110412     C                   ENDIF
053100110412     c                   else
053200000000     C*--------------------
053300000000     C* PERCENTUALE DA APPLICARE  (E' IN ALTERNATIVA ALL'IMPORTO UNIT.)
053400000000     C* SE NELLA VOCE NON C'E' "TIPO IMPONIBILE" NON DEVE ESSERCI LA %
053500000000     C*--------------------
053600110412     C     evdPER        IFGT      100
053700000000     C                   MOVEL     '1'           $ERR
053800000000     C                   MOVEL     MSG(41)       $MSG
053900110412     C                   EXSR      fine
054000110412     c                   leavesr
054100110412     C                   ENDIF
054200000000     C*--------------------
054300000000     C* IMPORTO MINIMO TARIFFA
054400000000     C*--------------------
054500940725     C* ACCETTO SOLO SE C'E' LA PERCENTUALE, ALTRIMENTI VUOTO
054600110412     C     evdPER        IFEQ      *ZERO
054700110412     C     evdIMI        andne     *ZERO
054800000000     C                   MOVEL     '1'           $ERR
054900000000     C                   MOVEL     MSG(32)       $MSG
055000110412     C                   EXSR      fine
055100110412     c                   leavesr
055200110412     C                   ENDIF
055300000000     C*--------------------
055400000000     C* IMPORTO MASSIMO TARIFFA
055500000000     C*--------------------
055600940725     C* ACCETTO SOLO SE C'E' LA PERCENTUALE, ALTRIMENTI VUOTO
055700110412     C     evdPER        IFEQ      *ZERO
055800110412     C     evdIMA        IFNE      *ZERO
055900000000     C                   MOVEL     '1'           $ERR
056000000000     C                   MOVEL     MSG(32)       $MSG
056100110412     C                   EXSR      fine
056200110412     c                   leavesr
056300110412     C                   ENDIF
056400110412     C                   ELSE
056500110412     C     evdIMA        IFLT      evdIMI
056600940725     C                   MOVEL     '1'           $ERR
056700940725     C                   MOVEL     MSG(36)       $MSG
056800110412     C                   EXSR      fine
056900110412     c                   leavesr
057000110412     C                   ENDIF
057100000000     C*--------------------
057200000000     C* SEGNO IMPORTO TARIFFA
057300000000     C*--------------------
057400110406IF  2C     evdSEG        IFEQ      *BLANKS
057500000000     C                   MOVEL     '1'           $ERR
057600000000     C                   MOVEL     MSG(33)       $MSG
057700110412     C                   EXSR      fine
057800110412     c                   leavesr
057900100422     c                   else
058000100422     c* se maggiorazione con voci da isolare le tariffe devono essere
058100100422     c* tutte positive o tutte negative a seconda del flag Applica S/N
058200100422     c                   if        flgsegtar = '1'
058300110406     C                   Z-ADD     evdVOC        KVTVOC
058400110406     C                   Z-ADD     evdPRG        KVTPRG
058500100422     c     keyevt        setll     ecevm01l
058600100422     c                   do        *hival
058700100422     c     keyevt        reade     ecevm01l
058800100422     c                   if        %eof(ecevm01l)
058900100422     c                   leave
059000100422     c                   end
059100110419     c*                  if        (evmapp = 'S' and evdseg = '+') or
059200110419     c*                            (evmapp = 'N' and evdseg = '-')
059300110419     C*                  MOVEL     '1'           $ERR
059400110419     C*                  MOVEL     MSG(42)       $MSG
059500110419     C*                  EXSR      fine
059600110419     c*                  leavesr
059700110419     c*                  end
059800100422     c                   enddo
059900100422     c                   end
060000110412     C                   ENDIF
060100110412     C                   ENDIF
060200110412     C                   ENDIF
060300000000     C*--------------------
060400000000     C* RAPPORTO PESO VOLUME
060500940725     C* LO ACCETTO SOLO SE NELLA VOCE LO CONSENTE IL TIPO RIFERIMENTO
060600000000     C*--------------------
060700110412     C     §2RRPV        IFEQ      'N'
060800110412     C     evdRPV        IFNE      *ZERO
060900000000     C                   MOVEL     '1'           $ERR
061000000000     C                   MOVEL     MSG(15)       $MSG
061100110412     C                   EXSR      fine
061200110412     c                   leavesr
061300110412     C                   ENDIF
061400110412     C                   ELSE
061500110412     C     EVTSPT        IFGT      '1'
061600110406     C     evdRPV        ANDEQ     *ZEROS
061700961202     C                   MOVEL     '1'           $ERR
061800961202     C                   MOVEL     MSG(26)       $MSG
061900110412     C                   EXSR      fine
062000110412     c                   leavesr
062100110412     C                   ENDIF
062200110412     C                   ENDIF
062300100331     C*--------------------
062400100331     C* SE VOCE DA ISOLARE PER MAGGIORAZ. si deve inserire solo cartello
062500100331     c* 100%
062600100331     C*--------------------
062700100422     C                   IF        evtvrv = '26' and
062800110406     c                             (evdttv <> 'C' or evdper <> 100
062900110406     c                             or evdseg <> '+')
063000100331     C                   MOVEL     '1'           $ERR
063100100331     C                   MOVEL     MSG(9)        $MSG
063200110412     C                   EXSR      fine
063300110412     c                   leavesr
063400100331     c                   end
063500000000     C*
063600000000     C                   ENDSR
063700970627     C*------------------------------------------------------------------------*
063800110412     C* fine - stampa
063900970627     C*------------------------------------------------------------------------*
064000110412     C     fine          BEGSR
064100970627     C*
064200110412     c                   if        *inof
064300110412     c                   except    testa
064400110412     c                   setoff                                       of
064500110412     c                   end
064600110418     c                   except    riga
064700110418     c                   eval      $err = '0'
064800110412     C*
064900110412     C                   ENDSR
065000110412     C*------------------------------------------------------------------------*
065100110412     C* CHKCAP - CONTROLLA IL CAP
065200110412     C*------------------------------------------------------------------------*
065300110412     C     CHKCAP        BEGSR
065400110412     C*
065500970627     C     *LIKE         DEFINE    I95NAR        WNAR
065600970627     C     *LIKE         DEFINE    I95CAP        WCAP
065700970627     C     *LIKE         DEFINE    I95DAT        WDAT
065800140522     C     *LIKE         DEFINE    O95VER        Wver
065900970627     C*
066000970627     C* CONTROLLA IL CAP
066100970627     C                   CLEAR                   DSSI95
066200970627     C                   MOVEL     *BLANKS       I95TLA
066300970627     C                   MOVEL     '3'           I95TCN
066400970627     C                   MOVEL     WNAR          I95NAR
066500970627     C                   MOVEL     WCAP          I95CAP
066600970627     C                   Z-ADD     WDAT          I95DAT
066700970627     C                   CALL      'TISI95R'
066800970627     C                   PARM                    DSSI95
066900970627IF  1C     O95ERR        IFNE      *BLANKS
067000140522     c* verifico se è un cap fittizio
067100140522     c     kazcpc        chain     azcpc01l
067200140522     c                   if        not %found(azcpc01l)
067300140522     C                   MOVEL     '1'           $ERR
067400140522E   1C                   ENDIF
067500970627E   1C                   ENDIF
067600970627     C*
067700970627     C                   ENDSR
067800100422     C*------------------------------------------------------------------------*
067900100422     C* CHKCTR - CONTROLLA CODICE TARIFFA
068000100422     C*          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
068100100422     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
068200100422     C*------------------------------------------------------------------------*
068300100422     C     CHKCTR        BEGSR
068400000000     C*
068500950719IF  2C     $CTR          IFGE      *ZEROS
068600110406     C                   MOVEL     evdCLI        KTAKSC
068700000000     C                   MOVEL     $CTR          KTACTR
068800000000     C*
068900941128     C     KEYTAM        CHAIN     TNTAM01L                           99
069000940722     C* SE INESISTENTE VA BENE LO STESSO: EVIDENZIO PERO' LA COSA
069100000000IF  4C     *IN99         IFEQ      *ON
069200110418     C     $CTR          ANDNE     '999'
069300110412     C                   MOVEL     '1'           $ERR
069400000000E   4C                   ENDIF
069500000000     C*
069600000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
069700000000X   3C                   ELSE
069800000000     C                   MOVEL     '1'           $ERR
069900000000E   3C                   ENDIF
070000000000     C*
070100000000     C                   ENDSR
070200000000     C*------------------------------------------------------------------------*
070300000000     C* CHKDAT - CONTROLLA LA DATA
070400000000     C*          INPUT --> $DAT:  DATA
070500000000     C*------------------------------------------------------------------------*
070600000000     C     CHKDAT        BEGSR
070700000000     C*
070800000000     C* OBBLIGATORIA
070900000000IF  1C     $DAT          IFEQ      *ZEROS
071000000000     C                   MOVEL     '1'           $ERR
071100000000     C                   MOVEL     MSG(8)        $MSG
071200000000X   1C                   ELSE
071300000000     C* CORRETTA
071400110418     C                   Z-ADD     *zeros        G02DAT
071500110418     C                   Z-ADD     $DAT          G02INV
071600110418     C                   MOVEL     '3'           G02ERR
071700000000     C                   CALL      'XSRDA8'
071800000000     C                   PARM                    WLBDAT
071900000000     C*
072000000000IF  2C     G02ERR        IFEQ      '1'
072100000000     C                   MOVEL     '1'           $ERR
072200000000     C                   MOVEL     MSG(8)        $MSG
072300000000X   2C                   ELSE
072400000000     C                   Z-ADD     G02DAT        $DAT
072500000000E   2C                   ENDIF
072600000000E   1C                   ENDIF
072700000000     C*
072800000000     C                   ENDSR
072900000000     C*------------------------------------------------------------------------*
073000000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
073100000000     C*          INPUT --> §COD: CODICE TABELLA
073200000000     C*                    $KEY: CODICE ELEMENTO
073300000000     C*------------------------------------------------------------------------*
073400000000     C     CHKTAB        BEGSR
073500000000     C*
073600110412     C     *LIKE         DEFINE    tblcod        §cod
073700110412     C     *LIKE         DEFINE    tblkey        §key
073800020923     C                   MOVEL     1             KTBKUT
073900000000     C                   MOVEL     §COD          KTBCOD
074000000000     C                   MOVE      *BLANKS       KTBKEY
074100000000     C                   MOVEL     §KEY          KTBKEY
074200000000     C     KEYTAB        CHAIN     TABEL00F                           99
074300000000     C     *IN99         IFEQ      '1'
074400000000     C     TBLFLG        ORNE      *BLANKS
074500000000     C                   MOVEL     '1'           $ERR
074600000000     C                   ENDIF
074700000000     C*
074800000000     C                   ENDSR
074900970627     C*------------------------------------------------------------------------*
075000970627     C* FINSR - OPERAZIONI FINALI
075100970627     C*------------------------------------------------------------------------*
075200970627     C     FINSR         BEGSR
075300970627     C*
075400971020     C* TISI95R
075500970704     C                   CLEAR                   DSSI95
075600970627     C                   MOVEL     'C'           I95TLA
075700970627     C                   CALL      'TISI95R'
075800970627     C                   PARM                    DSSI95
075900971020     C* FNLV55R
076000971020     C                   CLEAR                   DSLV55
076100971020     C                   MOVEL     'C'           D55TLA
076200971020     C                   CALL      'FNLV55R'
076300971020     C                   PARM                    DSLV55
076400000727     C* TIBS69R
076500000727     C                   CLEAR                   BS69DS
076600000727     C                   CLEAR                   ACODS
076700000727     C                   CLEAR                   INDDS
076800000727     C                   CLEAR                   CLPDS
076900000727     C                   CLEAR                   CLSDS
077000000727     C                   MOVEL     'C'           I69TLA
077100000727     C                   MOVEL     KNSIF         I69SIF
077200000727     C                   CALL      'TIBS69R'
077300000727     C                   PARM                    BS69DS
077400000727     C                   PARM                    ACODS
077500000727     C                   PARM                    INDDS
077600000727     C                   PARM                    CLPDS
077700000727     C                   PARM                    CLSDS
077800970627     C*
077900970627     C                   ENDSR
078000020923     C*-------------------------------------------------------------------------
078100020923     C* REPERISCE I DATI UTENTE
078200020923     C*-------------------------------------------------------------------------
078300020923     C     REPDATIUTE    BEGSR
078400020923     C*
078500020923     C* INIZIALIZZA VARIABILI DI WRK
078600020923     C                   CLEAR                   TIBS34DS
078700020923     C                   CLEAR                   AZUTEDS
078800020923     C                   CLEAR                   DDATIUTE
078900020923     C*
079000020923     C     *DTAARA       DEFINE    §azute        azuteds
079100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
079200020923     C                   IN(E)     *DTAARA
079300020923if  1C                   IF        %Error
079400020923     c                   EVAL      I34Tla = 'L'
079500020923     C                   CALL      'TIBS34R'
079600020923     C                   PARM                    Tibs34Ds
079700020923     C                   IN        *DTAARA
079800110412     C                   ENDIF
079900020923     C                   ENDSR
080000000000     C*------------------------------------------------------------------------*
080100000000     C*  *INZSR - ROUTINE INIZIALE
080200000000     C*------------------------------------------------------------------------*
080300000000     C     *INZSR        BEGSR
080400000000     C*
080500000000     C* RICEVIMENTO PARAMETRI
080600000000     C     *ENTRY        PLIST
080700000000     C                   PARM                    KPJBA
080800110412     C                   MOVEL     KPJBU         lib              10
080900110412     c                   seton                                        of
081000000000     C*
081100000000     C* DEFINIZIONE VARIABILI
081200000000     C     *LIKE         DEFINE    ORGFIL        KRGFIL
081300970627     C     *LIKE         DEFINE    TAMKSC        KTAKSC
081400000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
081500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
081600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
081700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
081800000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC
081900000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
082000000000     C     *LIKE         DEFINE    EVDTTV        KVDTTV
082100000000     C     *LIKE         DEFINE    EVDFT1        KVDFT1
082200000000     C     *LIKE         DEFINE    EVDFT2        KVDFT2
082300000000     C     *LIKE         DEFINE    EVDCLI        KVDCLI
082400000000     C     *LIKE         DEFINE    EVDCTR        KVDCTR
082500000000     C     *LIKE         DEFINE    EVDCTS        KVDCTS
082600131128     C     *LIKE         DEFINE    EVDnaz        KVDnaz
082700131128     C     *LIKE         DEFINE    EVDCAd        KVDCAd
082800000000     C     *LIKE         DEFINE    EVDDDT        KVDDDT
082900000000     C     *LIKE         DEFINE    EVDSGI        KVDSGI
083000000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
083100000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
083200000000     C*
083300131128     C* CHIAVE LETTURA ECEVD06L - COMPLETA
083400000000     C     KEYEVD        KLIST
083500000000     C                   KFLD                    KVDVOC
083600000000     C                   KFLD                    KVDPRG
083700000000     C                   KFLD                    KVDTTV
083800000000     C                   KFLD                    KVDFT1
083900000000     C                   KFLD                    KVDFT2
084000000000     C                   KFLD                    KVDCLI
084100000000     C                   KFLD                    KVDCTR
084200000000     C                   KFLD                    KVDCTS
084300131128     C                   KFLD                    KVDnaz
084400131128     C                   KFLD                    KVDcad
084500000000     C                   KFLD                    KVDDDT
084600000000     C                   KFLD                    KVDSGI
084700000000     C*
084800131128     C* CHIAVE LETTURA ECEVD06L - PARZIALE
084900000000     C     KE2EVD        KLIST
085000000000     C                   KFLD                    KVDVOC
085100000000     C                   KFLD                    KVDPRG
085200000000     C                   KFLD                    KVDTTV
085300000000     C                   KFLD                    KVDFT1
085400000000     C                   KFLD                    KVDFT2
085500000000     C                   KFLD                    KVDCLI
085600000000     C                   KFLD                    KVDCTR
085700000000     C                   KFLD                    KVDCTS
085800131128     C                   KFLD                    KVDnaz
085900131128     C                   KFLD                    KVDcad
086000000000     C*
086100131128     C* CHIAVE LETTURA ECEVD06L - COMPLETA
086200000000     C     KE3EVD        KLIST
086300000000     C                   KFLD                    KVDVOC
086400000000     C                   KFLD                    KVDPRG
086500000000     C                   KFLD                    KVDTTV
086600000000     C*
086700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
086800000000     C     KEYTAB        KLIST
086900000000     C                   KFLD                    KTBKUT
087000000000     C                   KFLD                    KTBCOD
087100000000     C                   KFLD                    KTBKEY
087200000000     C*
087300941128     C* CHIAVE LETTURA TNTAM01L - PARZIALE
087400000000     C     KEYTAM        KLIST
087500000000     C                   KFLD                    KTAKSC
087600000000     C                   KFLD                    KTACTR
087700000000     C*
087800000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
087900000000     C     KEYEVT        KLIST
088000000000     C                   KFLD                    KVTVOC
088100940728     C                   KFLD                    KVTPRG
088200971020     C*
088300140522     C* CHIAVE LETTURA azcpc01L - COMPLETA
088400140522     C     Kazcpc        KLIST
088500140522     C                   KFLD                    wver
088600140522     C                   KFLD                    wnar
088700140522     C                   KFLD                    wcap
088800140522      *
088900140522     c* reperisco la versione del cappario
089000140522     C                   CLEAR                   wver
089100140522     C                   CLEAR                   DSSI95
089200140522     C                   MOVEL     '3'           I95TCN
089300140522     C                   MOVEL(p)  '40100'       I95CAP
089400140522     C                   Z-ADD     WDAT          I95DAT
089500140522     C                   CALL      'TISI95R'
089600140522     C                   PARM                    DSSI95
089700140522IF  1C     O95ERR        IFeq      *BLANKS
089800140522     C                   MOVEL     o95ver        wver
089900140522E   1C                   ENDIF
090000971020     C* REPERISCE LA DATA CORRENTE
090100971020     C                   TIME                    N14              14 0
090200971020     C                   MOVE      N14           N8                8 0
090300110419     C                   MOVEl     N14           N6                6 0
090400971020     C                   Z-ADD     N8            G02DAT
090500971020     C                   Z-ADD     *ZEROS        G02INV
090600971020     C                   MOVEL     '0'           G02ERR
090700971020     C                   CALL      'XSRDA8'
090800971020     C                   PARM                    WLBDAT
090900971020     C                   Z-ADD     G02INV        DATCOR            8 0
091000000000     C*
091100000000     C                   ENDSR
091200110419     oprtf198   e            testa             1
091300110505     o                                          100 'Controllo tariffe C/E'
091400110412     o                       lib                + 1
091500110419     o                       n8                 +10 '  /  /    '
091600110419     o                       n6                 + 2 '  :  :  '
091700110419     o                       page               198
091800110419     o          e            testa             3  4
091900110419     o                                          + 0 'Voce'
092000110419     o                                            9 'T'
092100110419     o                                           17 'Filiale'
092200110419     o                                           25 'Cliente'
092300110419     o                                           29 'Tar'
092400131203     o                                           36 'Naz'
092500131203     o                                           42 'C.A.P'
092600131203     o                                           59 'Decorrenza'
092700131203     o                                           70 'Scadenza'
092800131203     o                                           85 'Scaglione I.'
092900131203     o                                          100 'Scaglione F.'
093000131203     o                                          115 'I. Unitario'
093100131203     o                                          130 'I. Minimo'
093200131203     o                                          145 'I. Massimo'
093300131203     o                                          147 'S'
093400131203     o                                          154 '%'
093500131203     o                                          159 'RPV'
093600131203     o                                          170 'Inserimen.'
093700131203     o                                          181 'Modifica'
093800131203     o                                          194 'Trasmissione'
093900131203     o                                          196 'A'
094000110418     o          e            riga        1
094100110418     o                       EVDVOC              +0
094200110418     o                       EVDPRG              +1
094300110418     o                       EVDTTV              +1
094400110418     o                       EVDFT1              +1
094500110418     o                       EVDFT2              +1
094600110418     o                       EVDCLI              +1
094700110418     o                       EVDCTR              +1
094800110418     o                       EVDCTS              +1
094900131203     o                       EVDnaz              +1
095000131203     o                       EVDcad              +1
095100110418     o                       EVDDDT              +1 '    /  /  '
095200110418     o                       EVDDST              +1 '    /  /  '
095300110418     o                       EVDSGI        4     +1
095400110418     o                       EVDSGF        4     +1
095500110418     o                       EVDITR        4     +1
095600110418     o                       EVDIMI        4     +1
095700110418     o                       EVDIMA        4     +1
095800110418     o                       EVDSEG              +1
095900110418     o                       EVDPER        4     +1
096000110418     o                       EVDRPV        4     +1
096100110418     o                       EVDDIR              +1 '    /  /  '
096200110418     o                       EVDDUV              +1 '    /  /  '
096300110418     o                       EVDFTR              +1
096400110418     o                       EVDDTR              +1 '    /  /  '
096500110418     o                       EVDATB              +1
096600110418     o          e            riga        1
096700110412     o                       $msg                78
096800981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
096900110406                                                                               1
097000110406                                                                               2
097100110406                                                                               3
097200110406                                                                               4
097300110406                                                                               5
097400110406                                                                               6
097500110406Immettere codice voce esistente                                                7
097600110406Immettere data formalmente valida                                              8
097700110406La tariffa per voce isolamento maggioraz. deve essere CARTELLO          100%   9
097800110406Immettere codice C.A.P. esistente e non annullato                             10
097900110406Tariffa già esistente                                                         11
098000110420Immettere codice tariffa valido
098100110406Immettere scaglione tariffa                                                   13
098200110406Immettere codice filiale esistente                                            14
098300110406Non immettere il rapporto peso volume per il tipo riferimento          voce   15
098400110406Immettere codice tassazione esistente e non annullato                         16
098500110406                                                                              17
098600110406                                                                              18
098700110406                                                                              19
098800110406                                                                              20
098900110406Richiesta funzione per voce già esistente                                     21
099000110406                                                                              22
099100110406La data scadenza deve essere maggiore o uguale della decorrenza               23
099200110406Per il Tipo Riferimento Voce non e' possibile aggiungere piu' voci            24
099300110406Immettere importo unitario solo se tipo tariffa a Quantità/Imp fisso          25
099400110406Rapporto peso volume obbligatorio                                             26
099500110406La validità della tariffa si sovrappone ad una già in decorrenza              27
099600110406Immettere filiali non uguali                                                  28
099700141203Se maggioraz. con tipo tariffa obbligo % a 0 o 100 positiva                   29
099800110406Immettere codice cliente esistente e non annullato                            30
099900110406La tariffa x voce isolamento maggior. deve avere scadenza 31/12/2039          31
100000110406Non immettere importo se non presente la percentuale                          32
100100110406Immettere il segno per la tariffa                                             33
100200110406Se una filiale e' 1° livello l'altra deve appartenere all'area                34
100300110406Immettere codice C.A.P. esistente e non annullato                             35
100400110406Immettere importo minimo inferiore al massimo                                 36
100500141203Per le maggioraz. solo Tipo tariffa abilitato e no Tipo ricerca         scagl.36
100600110406Per il riferimento tariffa, non si puo' immettere la filiale                  38
100700110406Il periodo immesso non e' compreso in quello della voce                       39
100800110406Nella voce non vi e' alcun riferimento per il tipo tariffa                    40
100900110406Immettere una percentuale valida                                              41
101000110406La voce di isolamento ha il flag Applica incongruente con il segno            42
101100110406
101200940831* CASO PARTICOLARE *                                                          44
101300140522*** INESISTENTE ***                                                           45
101400140522TREC64R- Scaglione incongruente con scaglioni esistenti                       46
101500140522TREC64R- Imserire un CAP generico correttamente                               47
101600140522TREC64R- Codice Nazione Inesistente                                           48
