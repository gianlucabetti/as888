000100020319      /TITLE stampa Rettifiche conteggi
000200120907      *-----------------------------------------------
000300120907      *  Nell'ottica di eliminare la stampa ed il consumo di carta,
000400120907      *   è stato tolto sul video i parametri relativi alla ristampa rendendoli
000500120907      *   di tipo HIDDEN. ossia il programma non viene modificato ma
000600120907      *   semplicemente NON vengono gestiti gli input di tali campi e di
000700120907      *   conseguenza non viene eseguita la parte di programma x la ristampa.
000800120907      *-----------------------------------------------
000900011221
001000930111     H DECEDIT('0,') DATEDIT(*DMY.)
001100011221
001200020319     Fazorg01l  IF   E           K DISK
001300020319     Fficn41d   CF   E             WORKSTN
001400011221     D                SDS
001500011221     D PgmNam            *PROC
001600020319     D ficn41ds      E DS
001700891030     D KPJBA         E DS
001800020318     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001900020318     D  LIN                    1     90  0
002000020318     D                                     DIM(30)                              FIL. COMODO
002100020318     D ut§dse        E DS                  extname(ut§dse0f)
002200020318     D CNCR80        E DS
002300020319     D ficn70ds      E DS
002400020319     D daut          E DS
002500020318     D WLBDAT          DS
002600020318     D  G02DAT                 1      8  0
002700020318     D  G02INV                 9     16  0
002800020318     D  G02ERR                17     17
002900020318     D  G02TGI                18     22  0
003000020308     D datiso          S               d
003100021111     D meno5           S               d
003200020308     D L1              S              3  0 DIM(30)
003300020308     D L6              S              3  0 DIM(30)
003400921222     C                   EXSR      DEFCAM
003500921222     C     *INKC         DOWEQ     '0'
003600020318     c* primo video
003700011221     C                   IF        *IN99
003800011221     C                   EVAL      *IN99 = *OFF
003900011221     C                   WRITE     CA81D1
004000011221     C                   EVAL      *IN99 = *ON
004100011221     C                   ENDIF
004200020318     c* se chiamato da menù emetto la prima videata di richiesta parametri
004300921222     C                   EXFMT     CA81D1
004400020318     c* fine
004500020318     C                   IF        *INKC
004600020318     c                   leave
004700020318     C                   ENDIF
004800020318     c* controlli
004900921222     C                   EXSR      CTRD1
005000020318     c   99              iter
005100020319     c* conferma
005200020319     C                   if        *inkf
005300020322     c                   move      pov           §41fil
005400020321     c                   movel     dettv         §41det
005500020322     c* per la stampa mandato pagamento sempre dettaglio
005600020322     c                   if        dtfttv <> 0
005700020322     c                   movel     'S'           §41det
005800020322     c                   end
005900020319     c                   movel(p)  ficn41ds      kpjbu
006000020319     c     knmus         ifeq      *all'1'
006100020329     c                   call      'FICN42C'
006200020319     c                   parm                    kpjba
006300020319     c                   else
006400020322     C                   MOVEL     'FN42'        KCOAZ
006500020319     C                   CALL      'BCH10'
006600020319     C                   PARM                    KPJBA
006700020319     c                   end
006800020319     c                   leave
006900020319     C                   ENDIF
007000020318     C                   ENDDO
007100921222     C                   SETON                                        LR
007200921222     C**************************************************************************
007300921222     C* CONTROLLI VIDEO1
007400020308     C**************************************************************************
007500921223     C     CTRD1         BEGSR                                                  *
007600921224     C                   SETOFF                                       99
007700020308     C* Filiale Padroncino
007800020321     c* per i secondi livelli il punto operativo è obbligatorio
007900020321     C     pov           IFEQ      0
008000020701     c*    cdu           andne     simfel
008100020701     c     SimPOU        andne     simfel
008200020321     C                   SETON                                        0199
008300020321     C                   End
008400020308     C* Verifico esistenza filiale padroncino: se REM su £6, se SIMFEL
008500020308     C* su £1
008600020321     C     pov           IFne      0
008700020701     C*    REM           IFEQ      'REM'
008800020701     C*    REMFIL        ANDGT     *ZEROS
008900020701     C                   IF        SimTpP = '2' OR SimTpP = *BLANK
009000020308     C     pov           LOOKUP    L6                                     20
009100020308     C  n20              SETON                                        0199
009200040309     C*                  ELSE
009300040309     C*    pov           LOOKUP    L1                                     20
009400040309     C* N20              SETON                                        0199
009500020308     C                   END
009600020321     C                   END
009700020319     c* decodifico
009800020321     c                   if        pov <> 0
009900020319     c     pov           chain     azorg01l
010000020319     c                   if        %found(azorg01l)
010100020319     c                   movel     orgdes        podesv
010200020319     c                   end
010300020321     c                   else
010400020321     c                   movel     *blanks       podesv
010500020321     c                   end
010501120918     C* Data conferma dal
010502120918    1C     dtfttd        IFGT      0
010503120918     C                   Z-ADD     dtfttd        G02DAT
010504120918     C                   MOVEL     *BLANK        G02ERR
010505120918     C                   CALL      'XSRDA8'
010506120918     C                   PARM                    WLBDAT
010507120918    2C     G02ERR        IFEQ      '1'
010508120918     C                   SETON                                        0799
010509120918    2C                   Else
010510120918     C                   move      G02dat        dtfttd
010511120918     C                   move      G02INV        datiso
010512120918     c                   move      datiso        §41ddd
010513120918    1C                   ENDIF
010514120918    1C                   ENDIF
010600020319     C* Data conferma
010700020319    1C     dtfttv        IFGT      0
010800020319     C                   Z-ADD     dtfttv        G02DAT
010900020318     C                   MOVEL     *BLANK        G02ERR
011000020318     C                   CALL      'XSRDA8'
011100020318     C                   PARM                    WLBDAT
011200020318    2C     G02ERR        IFEQ      '1'
011300020319     C                   SETON                                        0299
011400020319    2C                   Else
011500020319     C                   move      G02dat        dtfttv
011600020318     C                   move      G02INV        datiso
011700120918     c                   move      datiso        §41add
011800020319    1C                   ENDIF
011900020308    1C                   ENDIF
011901120918      **
011902120918     C* Range di date non in ordine
011903120918     c                   if        §41add < §41ddd
011904120918     C                   SETON                                        0899
011905120918    1C                   ENDIF
011906120918      **
012000020319     C* Data dal
012100020319    1C     daldds        IFGT      0
012200020319     C                   Z-ADD     daldds        G02DAT
012300020319     C                   MOVEL     *BLANK        G02ERR
012400020319     C                   CALL      'XSRDA8'
012500020319     C                   PARM                    WLBDAT
012600020319    2C     G02ERR        IFEQ      '1'
012700120918     C                   SETON                                        0399
012800020319    2C                   Else
012900020319     C                   move      G02dat        daldds
013000020319     C                   move      G02INV        datiso
013100021111     c* se oggi -
013200021111     c                   if        datiso < meno5
013300021112     C                   SETON                                        0699
013400021111     c                   else
013500020319     c                   move      datiso        §41dds
013600020319    1C                   ENDIF
013700021111    1C                   ENDIF
013800020319    1C                   ENDIF
013900020319     C* Data al
014000020319    1C     aldds         IFGT      0
014100020319     C                   Z-ADD     aldds         G02DAT
014200020319     C                   MOVEL     *BLANK        G02ERR
014300020319     C                   CALL      'XSRDA8'
014400020319     C                   PARM                    WLBDAT
014500020319    2C     G02ERR        IFEQ      '1'
014600020319     C                   SETON                                        0499
014700020319    2C                   Else
014800020319     C                   move      G02dat        aldds
014900020319     C                   move      G02INV        datiso
015000020319     c                   move      datiso        §41ads
015100020319    1C                   ENDIF
015200020319    1C                   ENDIF
015300020321     C*
015400020321     C                   IF        §41ads < §41dds
015500020321     C                   SETON                                        049903
015600020321    1C                   ENDIF
015700020321     C*
015800020321     C                   IF        dtfttv<>0 and (daldds<>0 or aldds<>0)
015900020321     C                   SETON                                        049903
016000020321     C                   SETON                                        02
016100020321    1C                   ENDIF
016200021127     C                   IF        dtfttv=0 and (daldds=0 or aldds=0)
016300021127     C                   SETON                                        049903
016400021127    1C                   ENDIF
016500020322     C                   IF        dtfttv=0 and daldds=0 and aldds=0
016600020322     C                   SETON                                        049903
016700020322     C                   SETON                                        02
016800020322    1C                   ENDIF
016900020614     c* Controllo se abilitato alla stampa delle rettifiche
017000020701     c*                  movel     cdu           i70pog
017100020701     c                   Z-ADD     SimPOU        i70pog
017200020614     c                   move      pov           i70pge
017300020614     c                   movel     knmus         i70prf
017400020614     c                   call      'FICN70R'
017500020614     c                   parm                    ficn70ds
017600020614     c                   movel     o70uni        daut
017700020614     c                   if        dtfttv <> 0
017800020614     c                   if        §autret <> 'S'
017900020614     c                   seton                                        0599
018000020614     c                   end
018100020614     c                   else
018200020614     c                   if        §autcrs <> 'S' and §autret <> 'S'
018300020614     c                   seton                                        0599
018400020614     c                   end
018500020614     c                   end
018600011221     C*
018700921223     C                   ENDSR
018800020308     C**************************************************************************
018900921222     C* INIZIALIZZAZIONE
019000020308     C**************************************************************************
019100921223     C     DEFCAM        BEGSR                                                  *
019200921224     C                   SETOFF                                       98
019300921223     C     *ENTRY        PLIST                                                  *
019400921223     C                   PARM                    KPJBA                          *
019500020308     C                   Z-ADD     1             CODUT
019600020308     C                   CALL      'X§PARUT'
019700020308     C                   PARM                    UT§DSE
019800020322     C                   MOVEL     RAGUT         RSUT
019900020308     C                   MOVEL     REC80         CNCR80
020000020321     c                   clear                   ficn41ds
020100020308     C***
020200020308     C* CARICO TABELLA FILIALI GESTITE £1
020300020308     C***
020400020308     C                   CLEAR                   DSUL06
020500020308     C                   MOVE      '£1'          D06COD
020600020308     C                   MOVEL     SIMFEL        D06KEY
020700020308     C                   MOVEL     ' '           D06TLA
020800020308     C                   MOVEL     DSUL06        KPJBU
020900020308     C                   CALL      'TRUL06R'
021000020308     C                   PARM                    KPJBA
021100020308     C                   MOVEL     KPJBU         DSUL06
021200020308     C                   MOVEA     LIN           L1
021300020321     C                   EXSR      PUL01
021400020321     C***
021500020321     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
021600020321     C***
021700020321     C                   CLEAR                   DSUL06
021800020321     C                   MOVE      '£6'          D06COD
021900020701     C*                  MOVEL     CDU           D06KEY
022000020701     C                   MOVEL     SimPOU        D06KEY
022100020321     C                   MOVEL     'L'           D06TLA
022200020321     C                   MOVEL     DSUL06        KPJBU
022300020321     C                   CALL      'TRUL06R'
022400020321     C                   PARM                    KPJBA
022500020321     C                   MOVEL     KPJBU         DSUL06
022600020321     C                   MOVEA     LIN           L6
022700021111     C*                                                    *
022800021111     c                   move      *date         meno5
022900021111     c                   subdur    5:*d          meno5
023000021111     C*                                                    *
023100921223     C                   ENDSR
023200020308     C*---------------------------------------------------------------*
023300020308     C* PULISCO 1 FORMATO --------------------------------------------*
023400020308     C*---------------------------------------------------------------*
023500020308     C     PUL01         BEGSR
023600020308     C                   CLEAR                   pov
023700020319     C                   CLEAR                   podesv
023800020319     C                   CLEAR                   dtfttv
023900020319     C                   CLEAR                   daldds
024000020319     C                   CLEAR                   aldds
024100020701     C*                  Z-ADD     CDU           pov
024200020701     C                   Z-ADD     SimPOU        pov
024300020321     c                   movel     'N'           dettv
024400020308     C                   ENDSR
