000100000000     H*------------------------------------------------------------------------*
000200041202     H*         - LANCIO AGGIORNAMENTI SALDI CLIENTE DA GRU a BDG              *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
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
001900000000     D*-------------
002000990602     D* CONTROLLO DATE -XSRDA8-
002100000000     D*-------------
002200990602     D WLBDA8          DS                  INZ
002300990602     D  G08DAT                 1      8  0
002400990602     D  G08INV                 9     16  0
002500990602     D  G08ERR                17     17
002600990602     D  G08TGI                18     22  0
002700990602     D*------------------
002800990602     D* DS DATA GENERICA(8) IN GG/MM/AAAA
002900990602     D*------------------
003000990602     D                 DS
003100990602     D  DGIOR8                 1      2  0
003200990602     D  DMESE8                 3      4  0
003300990602     D  DANNO8                 5      8  0
003400990602     D  DDATA8                 1      8  0
003500000000     D*-------------
003600000000     D* ARCHITETTURA
003700000000     D*-------------
003800000000     D KPJBA         E DS
003900000000     D*-------------
004000000000     D* PARAMETRI DI PASSAGGIO CL DI AGGIORNAMENTO
004100000000     D*-------------
004200000000     D PARAM           DS                  INZ
004300041202     D  PARORD                 1      1
004400041202     D  PARFIL                 2      4  0
004500041202     D  PARKS1                 5     11  0
004600041202     D  PARDS1                12     19  0
004700041202     D  PARAA1                12     15  0
004800041202     D  PARMG1                16     19  0
004900041202     D  PARME1                16     17  0
005000041202     D  PARDS2                20     27  0
005100041202     D  PARAA2                20     23  0
005200041202     D  PARMG2                24     27  0
005300041202     D  PARME2                24     25  0
005400041202     D  PARDF1                28     35  0
005500041202     D  PARDF2                36     43  0
005600041202     D  PARKS2                44     50  0
005700041202     D  PARASI                51     54  0
005800130228     D  PARlib                55     64
005900000000     C*------------------------------------------------------------------------*
006000000000     C* MAIN LINE
006100000000     C*------------------------------------------------------------------------*
006200041202     C                   WRITE     O901DVT
006300000000     C*
006400041202     C                   do        *hival
006500000000     C                   EXSR      GESV1
006600041202     c   kc              leave
006700000000E   1C                   ENDDO
006800000000     C*
006900000000     C* FINE MAIN DEL PGM
007000000000     C                   SETON                                        LR
007100000000     C*------------------------------------------------------------------------*
007200000000     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
007300000000     C*------------------------------------------------------------------------*
007400000000     C     GESV1         BEGSR
007500000000     C*
007600000000     C                   EXFMT     O901DV1
007700041202     c* controllo
007800041202     c                   exsr      decodi
007900041202     c                   if        not *in99 and *inkf
008000000000     C                   EXSR      F06GES
008100041202     c                   move      '1'           *inkc
008200041202     c                   end
008300000000     C*
008400000000     C                   ENDSR
008500000000     C*------------------------------------------------------------------------*
008600000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
008700000000     C*------------------------------------------------------------------------*
008800000000     C     F06GES        BEGSR
008900000000     C* LANCIO PROGRAMMA DI STAMPA
009000041202     c                   clear                   param
009100041202     C                   MOVEL     V1CDAI        PARAA1
009200041202     C                   MOVE      V1CDAI        PARMe1
009300041202     C                   MOVEL     V1CDAF        PARAA2
009400041202     C                   MOVE      V1CDAF        PARMe2
009500130228     C                   MOVEL     'ECS2'        KCOAZ
009501130228     c                   movel     v1clib        parlib
009502130228     c                   z-add     v1casi        parasi
009600000000     C                   MOVEL     PARAM         KPJBU
009700000000     C                   CALL      'BCH10'
009800000000     C                   PARM                    KPJBA
009900000000     C*
010000000000     C                   ENDSR
010100000000     C*------------------------------------------------------------------------*
010200000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
010300000000     C*------------------------------------------------------------------------*
010400000000     C     DECODI        BEGSR
010500000000     C*
010600000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
010700041202     C                   SETOFF                                       606199
010701130228     C                   SETOFF                                       6263
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
012100990602     C                   MOVE      01            G08INV
012200990602     C                   Z-ADD     *ZEROS        G08DAT
012300990602     C                   MOVEL     '3'           G08ERR
012400990602     C                   CALL      'XSRDA8'
012500990602     C                   PARM                    WLBDA8
012600000000     C*
012700990602IF  2C     G08ERR        IFEQ      '1'
012800041202     C                   MOVEL     MSG(2)        MSGv
012900000000     C                   SETON                                        60
013000041202     c                   leave
013100000000E   1C                   ENDIF
013200000000     C*--------------------
013300000000     C* ANNO/MESE FINALE
013400000000     C*--------------------
013500000000     C* OBBLIGATORIO
013600000000IF  2C     V1CDAF        IFEQ      *ZEROS
013700041202     C                   MOVEL     MSG(1)        MSGv
013800000000     C                   SETON                                        61
013900041202     c                   leave
014000000000E   2C                   ENDIF
014100041202     C*
014200990602     C                   MOVEL     V1CDAF        G08INV
014300990602     C                   MOVE      01            G08INV
014400990602     C                   Z-ADD     *ZEROS        G08DAT
014500990602     C                   MOVEL     '3'           G08ERR
014600990602     C                   CALL      'XSRDA8'
014700990602     C                   PARM                    WLBDA8
014800000000     C*
014900990602IF  2C     G08ERR        IFEQ      '1'
015000041202     C                   MOVEL     MSG(2)        MSGv
015100000000     C                   SETON                                        61
015200041202     c                   leave
015300000000E   2C                   ENDIF
015400000000     C* MINORE/UGUALE DATA CORRENTE - 1
015500000000IF  2C     V1CDAF        IFGT      AMCOR
015600041202     C                   MOVEL     MSG(4)        MSGv
015700000000     C                   SETON                                        61
015800041202     c                   leave
015900000000E   1C                   ENDIF
016000000000     C* MAGGIORE/UGUALE DATA INIZIALE
016100000000IF  2C     V1CDAF        IFLT      V1CDAI
016200041202     C                   MOVEL     MSG(3)        MSGv
016300000000     C                   SETON                                        61
016400041202     c                   leave
016500000000E   1C                   ENDIF
016601130228     C* ANNO
016602130228IF  2C     V1CASI        IFLE      *year
016604130228     C                   MOVEL     MSG(5)        MSGv
016606130228     C                   SETON                                        6299
016607130228     C                   LEAVE
016608130228E   2C                   ENDIF
016609130228     C* ANNO
016610130228IF  2C     V1Clib        IFeq      ' '
016611130228     C                   MOVEL     MSG(6)        MSGv
016612130228     C                   SETON                                        6399
016613130228     C                   LEAVE
016614130228E   2C                   ENDIF
016615130228     c                   enddo
016700041202     c*
016800130228     c                   if        *in60 or *in61
016900041202     c                   seton                                        99
017000041202     c                   end
017100000000     C*
017200000000     C                   ENDSR
017300020923     C*-------------------------------------------------------------------------
017400020923     C* REPERISCE I DATI UTENTE
017500020923     C*-------------------------------------------------------------------------
017600020923     C     REPDATIUTE    BEGSR
017700020923     C*
017800020923     C* INIZIALIZZA VARIABILI DI WRK
017900020923     C                   CLEAR                   TIBS34DS
018000020923     C                   CLEAR                   AZUTEDS
018100020923     C                   CLEAR                   DDATIUTE
018200020923     C*
018300020923     C     *DTAARA       DEFINE    §azute        azuteds
018400020923     C     *DTAARA       DEFINE    §datiute      ddatiute
018500020923     C                   IN(E)     *DTAARA
018600020923if  1C                   IF        %Error
018700020923     c                   EVAL      I34Tla = 'L'
018800020923     C                   CALL      'TIBS34R'
018900020923     C                   PARM                    Tibs34Ds
019000020923     C                   IN        *DTAARA
019100020923e   1C                   ENDIF
019200020923     C*
019300020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
019400020923     C                   MOVEL(P)  RSUT          DSFIRS           20
019500020923     C*
019600020923     C                   ENDSR
019700000000     C*------------------------------------------------------------------------*
019800000000     C* *INZSR  - ROUTINE INIZIALE
019900000000     C*------------------------------------------------------------------------*
020000000000     C     *INZSR        BEGSR
020100000000     C*
020200000000     C* RICEVIMENTO PARAMETRI
020300000000     C     *ENTRY        PLIST
020400000000     C                   PARM                    KPJBA
020500000000     C*
020600000000     C* IMPOSTAZIONE INIZIALE VIDEO
020700000000     C                   EXSR      IMPVID
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*------------------------------------------------------------------------*
021100000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
021200000000     C*------------------------------------------------------------------------*
021300000000     C     IMPVID        BEGSR
021400990602     C*
021500990602     C                   TIME                    N14              14 0
021600990602     C                   MOVE      N14           N8                8 0
021700990602     C                   Z-ADD     N8            DDATA8
021800000000     C*--------------------
021900000000     C* ANNO/MESE INIZIALE (INIZIO DELL'ANNO)
022000000000     C*--------------------
022100990602     C                   Z-ADD     DANNO8        ANNO              4 0
022200990602     C                   Z-ADD     DMESE8        MESE              2 0
022300000000IF  1C     MESE          IFEQ      01
022400000000     C                   SUB       1             ANNO
022500000000X   1C                   ELSE
022600000000     C                   Z-ADD     01            MESE
022700000000E   1C                   ENDIF
022800000000     C                   MOVEL     ANNO          V1CDAI
022900000000     C                   MOVE      MESE          V1CDAI
023000000000     C*--------------------
023100000000     C* ANNO/MESE FINALE (ANNO/MESE CORRENTE - 1)
023200000000     C*--------------------
023300990602     C                   Z-ADD     DANNO8        ANNO
023400990602     C                   Z-ADD     DMESE8        MESE
023500000000     C                   SUB       1             MESE
023600000000IF  1C     MESE          IFEQ      *ZEROS
023700000000     C                   Z-ADD     12            MESE
023800000000     C                   SUB       1             ANNO
023900000000E   1C                   ENDIF
024000990602     C                   MOVEL     ANNO          V1CDAF
024100990602     C                   MOVE      MESE          V1CDAF
024200041202     C                   Z-ADD     V1CDAF        AMCOR             6 0
024300000000     C*
024400000000     C                   ENDSR
024500000000     O*------------------------------------------------------------------------*
024600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
024700041202Anno/mese obbligatorio                                                         1
024800041202Anno/mese errati                                                               2
024900041202Anno/mese finale minore dell'Anno/mese iniziale                                3
025000041202Anno/mese finale superiore alla data corrente                                  4
025100130228Anno errato
025200130228libreria obbligatoria
