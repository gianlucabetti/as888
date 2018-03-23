000100010418     H DECEDIT('0,') DATEDIT(*DMY.)
000200010418
000300010418      *****************************************************************
000400010418      *                                                               *
000500010418      *       STAMPA ANAGRAFICA RITIRI FISSI                          *
000600010418      *                                                               *
000700010418      *****************************************************************
000800010418
000900010418     FAZORG01L  IF   E           K DISK
001000010418     FFIOR21D   CF   E             WORKSTN
001100010418
001200020809     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             Msg video
001300020415     D L1              S              3  0 DIM(30)                              FIL MECCANIZZ.CTB
001400010418
001500010418     D KPJBA         E DS
001600010418     D FIOR21DS      E DS
001700010418
001800010418      * DS PER TRUL06R - CARICAMENTO £X
001900010418     D TRUL06DS      E DS
002000010418     D  LIN                    1     90  0
002100010418     D                                     DIM(30)                              FIL. COMODO
002200010418     D UT§DSE0F      E DS
002300010418     D CNCR80        E DS
002400010418
002500010418     D                SDS
002600010418     D  V1CPGM                 1     10
002700010418      *--------------------------------------------------------------**
002800010418      * INDICATORI                                                   **
002900010418      *--------------------------------------------------------------**
003000010418      *  01  - RICHIESTA RICERCA ALFABETICA
003100010418      *  02  - VARIAZIONE
003200020809      *  05  - SPROTEGGO P.O. IN GESTIONE
003300020809      *  06  - NON ABILITO TASTO FUNZIONALE F18
003400010418      *  28  - Visualizzo messaggio Errore
003500010418      *  30  - Comodo
003600020809      *  40  - ERRORE p.o. in gestione
003700010418      *--------------------------------------------------------------**
003800010418
003900010418     C                   CLEAR                   V1cMSG
004000010418     C                   MOVEL     '1'           V1oRD
004100010418     C                   clear                   V1POR
004200010418     C                   clear                   V1mp
004300010418     C                   CLEAR                   V1poe
004400010418     C                   CLEAR                   V1dpor
004500010418     C                   CLEAR                   V1dpoe
004600010418
004700010418     C                   do        *hival
004800010418     C                   EXFMT     FI21D01
004900010418     C                   SETOFF                                         28
005000010418
005100010418      * F3=FINE
005200010418     C                   if        *inkc = *on
005300010418     C                   leave
005400010418     C                   endif
005500020809
005600020809      * F18=Cambio P.O. in gestione
005700020809     c   Ks              Eval      *In05 = *On
005800020809     c   Ks              Iter
005900020809
006000020809      * 05 on --> controllo P.O. in gestione
006100020809     c                   If        *In05 = *On
006200020809     c                   ExSr      Sr_Contrpo
006300020809     c  n28              Eval      *In05 = *Off
006400020809     c   28              Iter
006500020809     c                   EndIf
006600010418      * Controllo
006700010418     C                   EXSR      CTRD02
006800000309     C  NKF
006900010418     COR 28              iter
007000010418
007100010418      * F6 = CONFERMA
007200010418     C                   CLEAR                   fior21ds
007300010418     C                   MOVEL     V1ord         d21ORD
007400010418     C                   Z-ADD     V1por         d21POR
007500010418     C                   MOVEL     V1mp          d21mp
007600010418     C                   Z-ADD     V1poe         d21poe
007700020809     c                   eval      d21pog = v1cfgs
007800010418     C                   MOVEL     'OR22'        KCOAZ
007900010418     C                   MOVEL(P)  fior21ds      KPJBU
008000010418     C                   CALL      'BCH10'
008100010418     C**!!!              CALL      'FIOR22R'
008200010418     C                   PARM                    KPJBA
008300010502     C                   leave
008400010418
008500010418     C                   enddo
008600010418
008700010418     C                   eval      *inlr = *on
008800010418
008900020809      *---------------------------------------------------------------*
009000020809      * Controllo il P.O. di Gestione
009100020809      *---------------------------------------------------------------*
009200020809     c     Sr_ContrPo    BegSr
009300020809
009400020809     c                   Setoff                                       40
009500020809     c                   Clear                   V1dfgs
009600020809
009700020809     c     V1cfgs        Lookup    L1                                     30
009800020809     c                   If        Not *In30
009900020809     c                   Seton                                        40  28
010000020809     c                   Movel     Msg(3)        V1cmsg
010100020809     c                   Else
010200020809
010300020809      * Decodifica
010400020809     c     V1cfgs        Chain     Azorg01L
010500020809     c                   If        %Found(Azorg01l) and ORGfva = *Blanks
010600020809     c                   Movel     orgDES        v1dfgs
010700020809     c                   Else
010800020809     c                   Seton                                        40  28
010900020809     c                   Movel     msg(3)        v1cmsg
011000020809     c                   EndIf
011100020809     c                   EndIf
011200020809
011300020809     c                   EndSr
011400010418      *---------------------------------------------------------------*
011500010418      *    Controlli videata                                          *
011600010418      *---------------------------------------------------------------*
011700961003     C     CTRD02        BEGSR
011800010418
011900000309     C                   SETOFF                                       0102
012000010418     C                   CLEAR                   V1dpor
012100010418     C                   CLEAR                   V1dpoe
012200010418
012300010418      * P.O. RITIRO: SE IMMESSO
012400010418     C                   if        v1por <> 0
012500010418     C     v1por         chain     azorg01l
012600010418     C                   if        %found(azorg01l)
012700010418     C                   movel(p)  orgdes        v1dpor
012800020415     C     v1por         LOOKUP    L1                                     30
012900010418     C                   if        *in30 = *off
013000010418     C                   seton                                        01  28
013100010418     C                   movel     msg(2)        v1cmsg
013200010418     C                   goto      endctrd02
013300010418     C                   endif
013400010418     C                   else
013500010418     C                   seton                                        01  28
013600010418     C                   movel     msg(2)        v1cmsg
013700010418     C                   goto      endctrd02
013800010418     C                   endif
013900010418     C                   endif
014000010418
014100010418      * PO CLIENTE SE IMMESSO
014200010418     C                   if        v1poe <> *zeros
014300010418     C     v1poe         chain     azorg01l
014400010418     C                   if        %found(azorg01l)
014500010418     C                   movel(p)  orgdes        v1dpoe
014600010418     C                   else
014700010418     C                   seton                                        02  28
014800010418     C                   movel     msg(1)        v1cmsg
014900010418     C                   goto      endctrd02
015000010418     C                   endif
015100010418     C                   endif
015200010418
015300010418     C     endctrd02     ENDSR
015400010418      *****************************************************************
015500010418      * ROUTINE INIZIALE
015600010418      *****************************************************************
015700961003     C     *INZSR        BEGSR
015800010418
015900961003     C     *ENTRY        PLIST
016000961003     C                   PARM                    KPJBA
016100010418
016200961003     C                   Z-ADD     1             CODUT
016300961003     C                   CALL      'X§PARUT'
016400010418     C                   PARM                    UT§DSE0f
016500961003     C                   MOVEL     RAGUT         V1CRSU
016600961008     C                   MOVEL     RAGUT         PARDUT           30
016700961003     C                   MOVEL     REC80         CNCR80
016800020808
016900020808      * Imposto il P.O. DI GESTIONE
017000020808     c                   If        Simtpp = '2' or Simtpp = *Blanks
017100020808     c                   Movel     Simpou        V1cfgs
017200020809     c                   Eval      *In06 = *On
017300020808     c                   Else
017400020808     c                   Movel     Simfel        V1cfgs
017500020808     c                   EndIf
017600020808      * Decodifico P.o. di gestione
017700020808     c     V1cfgs        Chain     Azorg01L
017800020808     c                   If        Not%Found(Azorg01l)
017900020808     c                   Clear                   V1dfgs
018000020808     c                   Else
018100020808     c                   Movel     OrgDES        V1dfgs
018200020808     c                   EndIf
018300010418
018400020415      * CARICO p.o.
018500971203     C     SIMFEL        IFGT      0
018600010418     C                   CLEAR                   trul06ds
018700020415     C                   MOVE      '£1'          D06COD
018800961009     C                   MOVEL     SIMFEL        D06KEY
018900010418     C                   MOVEL     trul06ds      KPJBU
019000010418
019100961009     C                   CALL      'TRUL06R'
019200961009     C                   PARM                    KPJBA
019300010418     C                   MOVEL     KPJBU         trul06ds
019400020415     C                   MOVEA     LIN           L1
019500971203     C                   ENDIF
019600010418
019700961003     C                   ENDSR
019800000317**
019900070418Immettere una filiale emissione valida                                        1
020000070418Filiale ritiro non gestibile                                                  2
020100070418Filale gestione errata                                                        3
