000100000000     H*------------------------------------------------------------------------*
000200041202     H*         - LANCIO AGGIORNAMENTI SALDI CLIENTE DA GRU a BDG              *
000300000000     H*------------------------------------------------------------------------*
000400140530     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800130228     Ftrecs15d  CF   E             WORKSTN
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200130228     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001300000000     D*-------------
001400020923     D* DS REPERIMENTO DATI UTENTE
001500020923     D*-------------------
001600020923     D TIBS34DS      E DS
001700020923     D DDATIUTE      E DS
001800020923     D AZUTEDS       E DS                  extname(AZUTE00F)
001900140530     d dataiso         s               d
002000140530     d dataeur         s               d   datfmt(*eur)
002100000000     D*-------------
002200990602     D* CONTROLLO DATE -XSRDA8-
002300000000     D*-------------
002400990602     D WLBDA8          DS                  INZ
002500990602     D  G08DAT                 1      8  0
002600990602     D  G08INV                 9     16  0
002700990602     D  G08ERR                17     17
002800990602     D  G08TGI                18     22  0
002900000000     D*-------------
003000000000     D* ARCHITETTURA
003100000000     D*-------------
003200000000     D KPJBA         E DS
003300000000     D*-------------
003400000000     D* PARAMETRI DI PASSAGGIO CL DI AGGIORNAMENTO
003500000000     D*-------------
003600000000     D PARAM           DS                  INZ
003700041202     D  PARORD                 1      1
003800041202     D  PARFIL                 2      4  0
003900041202     D  PARKS1                 5     11  0
004000041202     D  PARDS1                12     19  0
004100041202     D  PARAA1                12     15  0
004200041202     D  PARMG1                16     19  0
004300041202     D  PARME1                16     17  0
004400041202     D  PARDS2                20     27  0
004500041202     D  PARAA2                20     23  0
004600041202     D  PARMG2                24     27  0
004700041202     D  PARME2                24     25  0
004800041202     D  PARDF1                28     35  0
004900041202     D  PARDF2                36     43  0
005000041202     D  PARKS2                44     50  0
005100041202     D  PARASI                51     54  0
005200130228     D  PARlib                55     64
005300000000     C*------------------------------------------------------------------------*
005400000000     C* MAIN LINE
005500000000     C*------------------------------------------------------------------------*
005600041202     C                   WRITE     O901DVT
005700000000     C*
005800041202     C                   do        *hival
005900000000     C                   EXSR      GESV1
006000041202     c   kc              leave
006100000000E   1C                   ENDDO
006200000000     C*
006300000000     C* FINE MAIN DEL PGM
006400000000     C                   SETON                                        LR
006500000000     C*------------------------------------------------------------------------*
006600000000     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
006700000000     C*------------------------------------------------------------------------*
006800000000     C     GESV1         BEGSR
006900000000     C*
007000000000     C                   EXFMT     O901DV1
007100041202     c* controllo
007200041202     c                   exsr      decodi
007300140603     c                   if        not *in99 and (*inkf or *inkg)
007400000000     C                   EXSR      F06GES
007500041202     c                   move      '1'           *inkc
007600041202     c                   end
007700000000     C*
007800000000     C                   ENDSR
007900000000     C*------------------------------------------------------------------------*
008000000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
008100000000     C*------------------------------------------------------------------------*
008200000000     C     F06GES        BEGSR
008300000000     C* LANCIO PROGRAMMA DI STAMPA
008400041202     c                   clear                   param
008500140530     C                   MOVEL     V1CDAI        PARds1
008600140530     C                   MOVEL     V1CDAF        PARds2
008700130228     c                   movel     v1clib        parlib
008800130228     c                   z-add     v1casi        parasi
008900140603     C                   MOVEL     'ECS2'        KCOAZ
009000000000     C                   MOVEL     PARAM         KPJBU
009100140603     c                   if        *inkg
009200000000     C                   CALL      'BCH10'
009300000000     C                   PARM                    KPJBA
009400140603     c                   else
009500140603     C                   CALL      'TRECS12C'
009600140603     C                   PARM                    KPJBA
009700140603     c                   end
009800000000     C*
009900000000     C                   ENDSR
010000000000     C*------------------------------------------------------------------------*
010100000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
010200000000     C*------------------------------------------------------------------------*
010300000000     C     DECODI        BEGSR
010400000000     C*
010500000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
010600041202     C                   SETOFF                                       606199
010700130228     C                   SETOFF                                       6263
010800041202     c                   clear                   msgv
010900000000     C*--------------------
011000000000     C* ANNO/MESE INIZIALE
011100000000     C*--------------------
011200041202     c                   do
011300000000     C* OBBLIGATORIO
011400000000IF  1C     V1CDAI        IFEQ      *ZEROS
011500041202     C                   MOVEL     MSG(1)        MSGv
011600000000     C                   SETON                                        60
011700041202     c                   leave
011800000000E   1C                   ENDIF
011900041202     C*
012000990602     C                   MOVEL     V1CDAI        G08INV
012100990602     C                   Z-ADD     *ZEROS        G08DAT
012200990602     C                   MOVEL     '3'           G08ERR
012300990602     C                   CALL      'XSRDA8'
012400990602     C                   PARM                    WLBDA8
012500000000     C*
012600990602IF  2C     G08ERR        IFEQ      '1'
012700041202     C                   MOVEL     MSG(2)        MSGv
012800000000     C                   SETON                                        60
012900041202     c                   leave
013000000000E   1C                   ENDIF
013100000000     C*--------------------
013200000000     C* ANNO/MESE FINALE
013300000000     C*--------------------
013400000000     C* OBBLIGATORIO
013500000000IF  2C     V1CDAF        IFEQ      *ZEROS
013600041202     C                   MOVEL     MSG(1)        MSGv
013700000000     C                   SETON                                        61
013800041202     c                   leave
013900000000E   2C                   ENDIF
014000041202     C*
014100990602     C                   MOVEL     V1CDAF        G08INV
014200990602     C                   MOVE      01            G08INV
014300990602     C                   Z-ADD     *ZEROS        G08DAT
014400990602     C                   MOVEL     '3'           G08ERR
014500990602     C                   CALL      'XSRDA8'
014600990602     C                   PARM                    WLBDA8
014700000000     C*
014800990602IF  2C     G08ERR        IFEQ      '1'
014900041202     C                   MOVEL     MSG(2)        MSGv
015000000000     C                   SETON                                        61
015100041202     c                   leave
015200000000E   2C                   ENDIF
015300000000     C* MINORE/UGUALE DATA CORRENTE - 1
015400000000IF  2C     V1CDAF        IFGT      AMCOR
015500041202     C                   MOVEL     MSG(4)        MSGv
015600000000     C                   SETON                                        61
015700041202     c                   leave
015800000000E   1C                   ENDIF
015900000000     C* MAGGIORE/UGUALE DATA INIZIALE
016000000000IF  2C     V1CDAF        IFLT      V1CDAI
016100041202     C                   MOVEL     MSG(3)        MSGv
016200000000     C                   SETON                                        61
016300041202     c                   leave
016400000000E   1C                   ENDIF
016500130228     C* ANNO
016600140603IF  2C                   IF        v1casi <= *year and
016700140603     c                             %subst(V1Clib: 1: 6) <> 'BUDGET'
016800130228     C                   MOVEL     MSG(5)        MSGv
016900130228     C                   SETON                                        6299
017000130228     C                   LEAVE
017100130228E   2C                   ENDIF
017200130228     C* ANNO
017300130228IF  2C     V1Clib        IFeq      ' '
017400130228     C                   MOVEL     MSG(6)        MSGv
017500130228     C                   SETON                                        6399
017600130228     C                   LEAVE
017700130228E   2C                   ENDIF
017800130228     c                   enddo
017900041202     c*
018000130228     c                   if        *in60 or *in61
018100041202     c                   seton                                        99
018200041202     c                   end
018300000000     C*
018400000000     C                   ENDSR
018500020923     C*-------------------------------------------------------------------------
018600020923     C* REPERISCE I DATI UTENTE
018700020923     C*-------------------------------------------------------------------------
018800020923     C     REPDATIUTE    BEGSR
018900020923     C*
019000020923     C* INIZIALIZZA VARIABILI DI WRK
019100020923     C                   CLEAR                   TIBS34DS
019200020923     C                   CLEAR                   AZUTEDS
019300020923     C                   CLEAR                   DDATIUTE
019400020923     C*
019500020923     C     *DTAARA       DEFINE    §azute        azuteds
019600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
019700020923     C                   IN(E)     *DTAARA
019800020923if  1C                   IF        %Error
019900020923     c                   EVAL      I34Tla = 'L'
020000020923     C                   CALL      'TIBS34R'
020100020923     C                   PARM                    Tibs34Ds
020200020923     C                   IN        *DTAARA
020300020923e   1C                   ENDIF
020400020923     C*
020500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
020600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
020700020923     C*
020800020923     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000000000     C* *INZSR  - ROUTINE INIZIALE
021100000000     C*------------------------------------------------------------------------*
021200000000     C     *INZSR        BEGSR
021300000000     C*
021400000000     C* RICEVIMENTO PARAMETRI
021500000000     C     *ENTRY        PLIST
021600000000     C                   PARM                    KPJBA
021700000000     C*
021800000000     C* IMPOSTAZIONE INIZIALE VIDEO
021900000000     C                   EXSR      IMPVID
022000000000     C*
022100000000     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
022400000000     C*------------------------------------------------------------------------*
022500000000     C     IMPVID        BEGSR
022600990602     C*
022700990602     C                   TIME                    N14              14 0
022800990602     C                   MOVE      N14           N8                8 0
022900140530     C                   MOVEL     N8            dataeur
023000140530     C                   MOVEL     dataeur       dataiso
023100140530     C                   MOVEL     dataiso       v1cdai
023200140530     C                   MOVE      01            V1CDAI
023300000000     C*--------------------
023400000000     C* ANNO/MESE FINALE (ANNO/MESE CORRENTE - 1)
023500000000     C*--------------------
023600140530     c                   movel     v1cdai        dataiso
023700140530     c                   adddur    1:*m          dataiso
023800140530     c                   subdur    1:*d          dataiso
023900140530     c                   move      dataiso       v1cdaf
024000140530     C                   Z-ADD     V1CDAF        AMCOR             8 0
024100000000     C*
024200000000     C                   ENDSR
024300000000     O*------------------------------------------------------------------------*
024400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
024500041202Anno/mese obbligatorio                                                         1
024600041202Anno/mese errati                                                               2
024700041202Anno/mese finale minore dell'Anno/mese iniziale                                3
024800041202Anno/mese finale superiore alla data corrente                                  4
024900130228Anno errato
025000130228libreria obbligatoria
