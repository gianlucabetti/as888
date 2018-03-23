000100020327     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200020327
000300020327      *------------------------------------------------------------------------*
000400020327      *                                                                        *
000500020327      *    Imposta flag 7 campo CLSFLO da flag 10                              *
000600020327      *                                                                        *
000700020327      *------------------------------------------------------------------------*
000800020327
000900020327     ffncls00f  up   e           k disk
001000020328     fazorg01l  if   e           k disk
001100020327     ftabel00f  if   e           k disk
001200020327     fcnaco00f  uf   e           k disk
001300020327
001400020327     d kcod            s                   like(tblcod)
001500020327     d                                     inz('2F')
001600020327     d xx              s              3  0
001700020327     d wpo             s                   like(orgfil)
001800020327     d w001a           s              1
001900020327
002000020327     d tcuds           ds
002100020327     d  f1                     1      1
002200020327     d  f3                     3      3
002300020327     d  f2                     2      2
002400020327     d  f4                     4      4
002500020327     d  f56                    5      6
002600020327
002700020327     d s2f             s              1    dim(50)
002800020328     d l1              s              3  0 dim(30)
002900020327
003000020327     d og143         e ds
003100020327     d ds2f          e ds
003200020328     d trul06ds      e ds
003300020328     d  lin                    1     90  0 dim(30)
003400020327     d cncr80        e ds
003500020327     d ut§dse0f      e ds
003600020327     d  tcu                  398    697    dim(50)
003700020327     d  kcu                  698    847p 0 dim(50)  packeven
003800020328     d kpjba         e ds
003900020327
004000020327      * controllo il p.o. del cliente
004100020327     c                   movel     clsksc        wpo
004200020327     c                   eval      xx = 1
004300020327     c                   clear                   w001a
004400020327      * aggancio organigramma
004500020327     c     wpo           chain     azorg01l
004600020328     c                   if        not %found(azorg01l) or orgfva <> *blanks
004700020328     c                   clear                   orgde3
004800020328     c                   endif
004900020327     c                   eval      og143 = orgde3
005000020328
005100020327      * controllo il network
005200020328     c                   select                                                 b1
005300020328
005400020327      * network estero sempre 'S' nel byte 7 di clsflo
005500020327     c                   when      §ogntw = 'FED' or §ogntw = 'DPD' or
005600020327     c                             §ogntw = 'EEX' or §ogntw = 'EUP'
005700020327     c                   eval      %subst(clsflo:7:1) = 'S'
005800020328      * controllo se fa parte dell'area di partenza
005900020328     c     wpo           lookup    l1                                     99
006000020328     c                   if        *in99                                        b2
006100020327      * sflaggo cnaco
006200020327     c     kcnaco        chain     cnaco00f
006300020328     c                   if        %found(cnaco00f)                             b3
006400020327     c                   clear                   acoftr
006500020327     c                   update    cnaco000
006600020328     c                   endif                                                  e3
006700020328     c                   endif                                                  e2
006800020327      * aggiorno fncls
006900020327     c                   update    fncls000
007000020328
007100020327      * network non estero controllo byte 10 di clsflo
007200020328     c                   other                                                  x1
007300020328      * se impostato byte 10 clsflo
007400020328     c                   if        %subst(clsflo:10:1) <> *blanks               b2
007500020327     c                   eval      w001a = %subst(clsflo:10:1)
007600020327      * controllo se lingua estera
007700020327     c     w001a         lookup    s2f                                    99
007800020327      * imposto il nuovo byte 7 di clsflo
007900020328     c                   if        *in99                                        b3
008000020327     c                   eval      %subst(clsflo:7:1) = 'S'
008100020328      * controllo se fa parte dell'area di partenza
008200020328     c     wpo           lookup    l1                                     99
008300020328     c                   if        *in99                                        b4
008400020327      * sflaggo cnaco
008500020327     c     kcnaco        chain     cnaco00f
008600020328     c                   if        %found(cnaco00f)                             b5
008700020327     c                   clear                   acoftr
008800020327     c                   update    cnaco000
008900020328     c                   endif                                                  e5
009000020328     c                   endif                                                  e4
009100020327      * aggiorno fncls
009200020327     c                   update    fncls000
009300020328     c                   endif                                                  e3
009400020328     c                   endif                                                  e2
009500020328
009600020328     c                   endsl                                                  e1
009700020327
009800020327      *---------------------------------------------------------------*
009900020327      *   RICERCA CAPOCONTI
010000020327      *---------------------------------------------------------------*
010100020327     c     capoconti     begsr
010200020327
010300020327      *--- RICERCA CAPOCONTI
010400020327     c                   DO        50            XX
010500020327     c                   MOVE      TCU(XX)       TCUDS
010600020327     c                   IF        F56 = 'CG'
010700020327
010800020327     c     f4            COMP      '1'                                    21
010900020327     c     f4            COMP      '2'                                    22
011000020327     c     f4            COMP      '3'                                    23
011100020327     c     f4            COMP      '6'                                    27
011200020327      ** 1 CLIENTI   21    ** 2 FORNITORI 22    ** 3 AGENTI    23
011300020327     c     f3            comp      '0'                                242425
011400020327     c     f3            comp      'I'                                    26
011500020327      ** 0 ITALIA   25      ** 1 ESTERO   24    ** I CAPO CONTO IVA
011600020327     c                   if        *in21 = *on
011700020327     c   24              Z-ADD     KCU(XX)       KCE               4 0
011800020327     c   25              Z-ADD     KCU(XX)       KCI               4 0
011900020327     c                   end
012000020327     c                   if        *in22 = *on
012100020327     c   24              Z-ADD     KCU(XX)       KFE               4 0
012200020327     c   25              Z-ADD     KCU(XX)       KFI               4 0
012300020327     c                   end
012400020327     c                   if        *in23 = *on
012500020327     c   24              Z-ADD     KCU(XX)       KAE               4 0
012600020327     c   25              Z-ADD     KCU(XX)       KAI               4 0
012700020327     c                   end
012800020327     c   26              Z-ADD     KCU(XX)       KIVA              4 0
012900020327     c   27              Z-ADD     KCU(XX)       KBNA              4 0
013000020327
013100020327     c                   ENDIF
013200020327     c                   ENDDO
013300020327
013400020327     c                   ENDSR
013500020327
013600020327      *---------------------------------------------------------------*
013700020327      *   ROUTINE INIZIALE
013800020327      *---------------------------------------------------------------*
013900020327     c     *inzsr        begsr
014000020328
014100020405     c**   *entry        plist
014200020405     c**                 parm                    kpjba
014300020327
014400020327     c                   z-add     1             codut
014500020327     c                   call      'X§PARUT'
014600020327     c                   parm                    ut§dse0f
014700020327
014800020327     c                   movel     rec80         cncr80
014900020327      *  Ricerca capoconti
015000020327     c                   exsr      capoconti
015100020327
015200020328      *  Carico schiera P.O. gestiti
015300020328     c                   clear                   trul06ds
015400020328     c                   eval      d06tla = 'L'
015500020328     c                   move      '£1'          d06cod
015600020328     c                   movel     simfel        d06key
015700020328     c                   movel(p)  trul06ds      kpjbu
015800020328     c                   call      'TRUL06R'
015900020328     c                   parm                    kpjba
016000020328     c                   movel(p)  kpjbu         trul06ds
016100020328     c                   movea     lin           l1
016200020327
016300020327      *  Carico tabella 2F
016400020327     c                   clear                   xx
016500020327     c     ktab          setll     tabel00f
016600020327     c                   do        *hival
016700020327     c     ktab          reade     tabel00f
016800020327     c                   if        %eof(tabel00f)
016900020327     c                   leave
017000020327     c                   endif
017100020327     c                   movel     tbluni        ds2f
017200020327     c                   if        §2ftxt = 'E'
017300020327     c                   add       1             xx
017400020327     c                   movel     tblkey        s2f(xx)
017500020327     c                   endif
017600020327     c                   enddo
017700020327
017800020327     c     kcnaco        klist
017900020327     c                   kfld                    codut
018000020327     c                   kfld                    kci
018100020327     c                   kfld                    clsksc
018200020327
018300020327     c     ktab          klist
018400020327     c                   kfld                    codut
018500020327     c                   kfld                    kcod
018600020327
018700020327     c                   endsr
